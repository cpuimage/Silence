/* Silence effect for SoX
 * by Heikki Leinonen (heilei@iki.fi) 25.03.2001
 * Major Modifications by Chris Bagwell 06.08.2001
 * Minor addition by Donnie Smith 13.08.2003
 *
 * This effect can delete samples from the start of a sound file
 * until it sees a specified count of samples exceed a given threshold
 * (any of the channels).
 * This effect can also delete samples from the end of a sound file
 * when it sees a specified count of samples below a given threshold
 * (all channels).
 * It may also be used to delete samples anywhere in a sound file.
 * Thesholds can be given as either a percentage or in decibels.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#define FLOAT_SAMPLE 1

#ifndef min
#define min(a, b) ((a) > (b) ? (b) : (a))
#endif

#if FLOAT_SAMPLE
#ifndef sox_sample_t
#define sox_sample_t float
#endif
#else
#ifndef sox_sample_t
#define sox_sample_t short
#endif
#endif

enum SilenceMode {
    SILENCE_TRIM,
    SILENCE_TRIM_FLUSH,
    SILENCE_COPY,
    SILENCE_COPY_FLUSH,
    SILENCE_STOP
};

typedef struct {
    char start;
    int start_periods;
    int start_duration;
    double start_threshold;
    char start_unit; /* "d" for decibels or "%" for percent. */
    int restart;

    sox_sample_t *start_holdoff;
    int start_holdoff_offset;
    int start_holdoff_end;
    int start_found_periods;

    bool stop;
    int stop_periods;
    int stop_duration;
    double stop_threshold;
    char stop_unit;

    sox_sample_t *stop_holdoff;
    int stop_holdoff_offset;
    int stop_holdoff_end;
    int stop_found_periods;

    double *window;
    double *window_current;
    double *window_end;
    int window_size;
    double rms_sum;

    bool leave_silence;

    /* State Machine */
    char mode;
    int channels;
    uint32_t rate;
} SilenceCtx;

static void clear_rms(SilenceCtx *ctx) {
    memset(ctx->window, 0,
           ctx->window_size * sizeof(double));

    ctx->window_current = ctx->window;
    ctx->window_end = ctx->window + ctx->window_size;
    ctx->rms_sum = 0;
}

static int sox_silence_config(SilenceCtx *ctx,
                              bool leave_silence,//   [0,1] def: 0
                              int start_periods,//   [0,9000] def: 0
                              int stop_periods,//   [ -9000,    9000] def: 0
                              float start_duration_val,//   [0,9000] def: 0
                              float stop_duration_val,//   [0,9000] def: 0
                              float stop_threshold,//   [0,DBL_MAX] def: 0
                              float start_threshold,//   [0,DBL_MAX] def: 0
                              char start_unit, // "%" or "d"
                              char stop_unit// "%" or "d"
) {
    /* check for option switches */
    ctx->leave_silence = leave_silence;
    ctx->start = false;
    ctx->start_periods = start_periods;
    if (ctx->start_periods < 0) {
        // printf("Periods must not be negative");
        return -1;
    }
    if (ctx->start_periods > 0) {
        ctx->start = true;
        ctx->start_duration = ctx->rate * start_duration_val * ctx->channels;
        ctx->start_threshold = start_threshold;
        ctx->start_unit = start_unit;
    }

    ctx->stop = false;
    ctx->stop_periods = stop_periods;
    if (ctx->stop_periods < 0) {
        ctx->stop_periods = -ctx->stop_periods;
        ctx->restart = 1;
    } else
        ctx->restart = 0;
    ctx->stop = true;
    ctx->stop_duration = stop_duration_val * ctx->rate * ctx->channels;
    ctx->stop_threshold = stop_threshold;
    ctx->stop_unit = stop_unit;

    /* Error checking */
    if (ctx->start) {
        if ((ctx->start_unit != '%') && (ctx->start_unit != 'd')) {
            //   printf("Invalid unit specified");
            return -1;
        }
        if ((ctx->start_unit == '%') && ((ctx->start_threshold < 0.0)
                                         || (ctx->start_threshold > 100.0))) {
            //  printf("silence threshold should be between 0.0 and 100.0 %%");
            return -1;
        }
        if ((ctx->start_unit == 'd') && (ctx->start_threshold >= 0.0)) {
            //   printf("silence threshold should be less than 0.0 dB");
            return -1;
        }
    }

    if (ctx->stop) {
        if ((ctx->stop_unit != '%') && (ctx->stop_unit != 'd')) {
            //  printf("Invalid unit specified");
            return -1;
        }
        if ((ctx->stop_unit == '%') && ((ctx->stop_threshold < 0.0) ||
                                        (ctx->stop_threshold > 100.0))) {
            //   printf("silence threshold should be between 0.0 and 100.0 %%");
            return -1;
        }
        if ((ctx->stop_unit == 'd') && (ctx->stop_threshold >= 0.0)) {
            //  printf("silence threshold should be less than 0.0 dB");
            return -1;
        }
    }
    return 1;
}

static int sox_silence_start(SilenceCtx *ctx) {

    /* When you want to remove silence, small window sizes are
     * better or else RMS will look like non-silence at
     * aburpt changes from load to silence.
     */
    ctx->window_size = (ctx->rate / 50) *
                       ctx->channels;
    ctx->window = (double *) calloc(ctx->window_size, sizeof(double));

    clear_rms(ctx);

    if (ctx->start)
        ctx->mode = SILENCE_TRIM;
    else
        ctx->mode = SILENCE_COPY;

    ctx->start_holdoff = (sox_sample_t *) calloc(ctx->start_duration, sizeof(sox_sample_t));
    ctx->start_holdoff_offset = 0;
    ctx->start_holdoff_end = 0;
    ctx->start_found_periods = 0;

    ctx->stop_holdoff = (sox_sample_t *) calloc(ctx->stop_duration, sizeof(sox_sample_t));
    ctx->stop_holdoff_offset = 0;
    ctx->stop_holdoff_end = 0;
    ctx->stop_found_periods = 0;

    return 1;
}

static bool aboveThreshold(
        sox_sample_t value /* >= 0 */, double threshold, int unit) {
    /* When scaling low bit data, noise values got scaled way up */
    /* Only consider the original bits when looking for silence */
    double scaled_value = value;
    if (sizeof(sox_sample_t) == 2) {
        scaled_value /= (((unsigned) -1) >> (33 - (16)));
    }
    if (unit == '%')
        scaled_value *= 100;
    else if (unit == 'd')
        // linear_to_dB
        scaled_value = (log10(scaled_value) * 20);

    return scaled_value > threshold;
}

static sox_sample_t compute_rms(SilenceCtx *ctx, sox_sample_t sample) {

    double new_sum;
    sox_sample_t rms;

    new_sum = ctx->rms_sum;
    new_sum -= *ctx->window_current;
    new_sum += ((double) sample * (double) sample);

    rms = sqrt(new_sum / ctx->window_size);

    return (rms);
}

static void update_rms(SilenceCtx *ctx, sox_sample_t sample) {

    ctx->rms_sum -= *ctx->window_current;
    *ctx->window_current = ((double) sample * (double) sample);
    ctx->rms_sum += *ctx->window_current;

    ctx->window_current++;
    if (ctx->window_current >= ctx->window_end)
        ctx->window_current = ctx->window;
}

/* Process signed long samples from input to output. */
/* Return number of samples processed in isamp and osamp. */
static int sox_silence_flow(SilenceCtx *ctx, const sox_sample_t *input, sox_sample_t *output,
                            int *isamp, int *osamp) {
    int threshold;
    int i, j;
    int nrOfTicks; /* sometimes wide, sometimes non-wide samples */
    /* non-wide samples */

    int nrOfInSamplesRead = 0;
    int nrOfOutSamplesWritten = 0;

    switch (ctx->mode) {
        case SILENCE_TRIM:
            /* Reads and discards all input data until it detects a
             * sample that is above the specified threshold.  Turns on
             * copy mode when detected.
             * Need to make sure and copy input in groups of "channels" to
             * prevent getting buffers out of sync.
             * nrOfTicks counts wide samples here.
             */
        silence_trim:
            nrOfTicks = min((*isamp - nrOfInSamplesRead),
                            (*osamp - nrOfOutSamplesWritten)) /
                        ctx->channels;
            for (i = 0; i < nrOfTicks; i++) {
                threshold = 0;
                for (j = 0; j < ctx->channels; j++) {
                    threshold |= aboveThreshold(
                            compute_rms(ctx, input[j]),
                            ctx->start_threshold,
                            ctx->start_unit);
                }

                if (threshold) {
                    /* Add to holdoff buffer */
                    for (j = 0; j < ctx->channels; j++) {
                        update_rms(ctx, *input);
                        ctx->start_holdoff[
                                ctx->start_holdoff_end++] = *input++;

                    }
                    nrOfInSamplesRead += ctx->channels;
                    if (ctx->start_holdoff_end >=
                        ctx->start_duration) {
                        if (++ctx->start_found_periods >=
                            ctx->start_periods) {
                            ctx->mode = SILENCE_TRIM_FLUSH;
                            goto silence_trim_flush;
                        }
                        /* Trash holdoff buffer since its not
                         * needed.  Start looking again.
                         */
                        ctx->start_holdoff_offset = 0;
                        ctx->start_holdoff_end = 0;
                    }
                } else /* !above Threshold */
                {
                    ctx->start_holdoff_end = 0;
                    for (j = 0; j < ctx->channels; j++) {
                        update_rms(ctx, input[j]);
                    }
                    input += ctx->channels;
                    nrOfInSamplesRead += ctx->channels;
                }
            } /* for nrOfTicks */
            break;

        case SILENCE_TRIM_FLUSH:
            /* nrOfTicks counts non-wide samples here. */
        silence_trim_flush:
            nrOfTicks = min((ctx->start_holdoff_end -
                             ctx->start_holdoff_offset),
                            (*osamp - nrOfOutSamplesWritten));
            nrOfTicks -= nrOfTicks % ctx->channels;

            if (nrOfTicks > 0) {
                memcpy(output, &ctx->start_holdoff[ctx->start_holdoff_offset],
                       nrOfTicks * sizeof(sox_sample_t));
            }
            output += nrOfTicks;
            ctx->start_holdoff_offset += nrOfTicks;
            nrOfOutSamplesWritten += nrOfTicks;
            /* If fully drained holdoff then switch to copy mode */
            if (ctx->start_holdoff_offset == ctx->start_holdoff_end) {
                ctx->start_holdoff_offset = 0;
                ctx->start_holdoff_end = 0;
                ctx->mode = SILENCE_COPY;
                goto silence_copy;
            }
            break;

        case SILENCE_COPY:
            /* Attempts to copy samples into output buffer.
             *
             * Case B:
             * If not looking for silence to terminate copy then
             * blindly copy data into output buffer.
             *
             * Case A:
             *
             * Case 1a:
             * If previous silence was detect then see if input sample is
             * above threshold.  If found then flush out hold off buffer
             * and copy over to output buffer.
             *
             * Case 1b:
             * If no previous silence detect then see if input sample
             * is above threshold.  If found then copy directly
             * to output buffer.
             *
             * Case 2:
             * If not above threshold then silence is detect so
             * store in hold off buffer and do not write to output
             * buffer.  Even though it wasn't put in output
             * buffer, inform user that input was consumed.
             *
             * If hold off buffer is full after this then stop
             * copying data and discard data in hold off buffer.
             *
             * Special leave_silence logic:
             *
             * During this mode, go ahead and copy input
             * samples to output buffer instead of holdoff buffer
             * Then also short ciruit any flushes that would occur
             * when non-silence is detect since samples were already
             * copied.  This has the effect of always leaving
             * holdoff[] amount of silence but deleting any
             * beyond that amount.
             *
             * nrOfTicks counts wide samples here.
             */
        silence_copy:
            nrOfTicks = min((*isamp - nrOfInSamplesRead),
                            (*osamp - nrOfOutSamplesWritten)) /
                        ctx->channels;
            if (ctx->stop) {
                /* Case A */
                for (i = 0; i < nrOfTicks; i++) {
                    threshold = 1;
                    for (j = 0; j < ctx->channels; j++) {
                        threshold &= aboveThreshold(
                                compute_rms(ctx, input[j]),
                                ctx->stop_threshold,
                                ctx->stop_unit);
                    }

                    /* Case 1a
                     * If above threshold, check to see if we where holding
                     * off previously.  If so then flush this buffer.
                     * We haven't incremented any pointers yet so nothing
                     * is lost.
                     *
                     * If user wants to leave_silence, then we
                     * were already copying the data and so no
                     * need to flush the old data.  Just resume
                     * copying as if we were not holding off.
                     */
                    if (threshold && ctx->stop_holdoff_end
                        && !ctx->leave_silence) {
                        ctx->mode = SILENCE_COPY_FLUSH;
                        goto silence_copy_flush;
                    }
                        /* Case 1b */
                    else if (threshold) {
                        /* Not holding off so copy into output buffer */
                        for (j = 0; j < ctx->channels; j++) {
                            update_rms(ctx, *input);
                            *output++ = *input++;
                        }
                        nrOfInSamplesRead += ctx->channels;
                        nrOfOutSamplesWritten += ctx->channels;
                    }
                        /* Case 2 */
                    else {
                        /* Add to holdoff buffer */
                        for (j = 0; j < ctx->channels; j++) {
                            update_rms(ctx, *input);
                            if (ctx->leave_silence) {
                                *output++ = *input;
                                nrOfOutSamplesWritten++;
                            }
                            ctx->stop_holdoff[
                                    ctx->stop_holdoff_end++] = *input++;

                        }
                        nrOfInSamplesRead += ctx->channels;
                        /* Check if holdoff buffer is greater than duration
                         */
                        if (ctx->stop_holdoff_end >=
                            ctx->stop_duration * ctx->channels) {
                            /* Increment found counter and see if this
                             * is the last period.  If so then exit.
                             */
                            if (++ctx->stop_found_periods >=
                                ctx->stop_periods) {
                                ctx->stop_holdoff_offset = 0;
                                ctx->stop_holdoff_end = 0;
                                if (!ctx->restart) {

                                    ctx->mode = SILENCE_STOP;
                                    goto silence_stop;
                                } else {
                                    ctx->stop_found_periods = 0;
                                    ctx->start_found_periods = 0;
                                    ctx->start_holdoff_offset = 0;
                                    ctx->start_holdoff_end = 0;
                                    clear_rms(ctx);
                                    ctx->mode = SILENCE_TRIM;

                                    goto silence_trim;
                                }
                            } else {
                                /* Flush this buffer and start
                                 * looking again.
                                 */
                                ctx->mode = SILENCE_COPY_FLUSH;
                                goto silence_copy_flush;
                            }
                            break;
                        } /* Filled holdoff buffer */
                    } /* Detected silence */
                } /* For # of samples */
            } /* Trimming off backend */
            else /* !(ctx->stop) */
            {
                /* Case B */
                memcpy(output, input, sizeof(sox_sample_t) * nrOfTicks *
                                      ctx->channels);
                nrOfInSamplesRead += (nrOfTicks * ctx->channels);
                nrOfOutSamplesWritten += (nrOfTicks * ctx->channels);
            }
            break;

        case SILENCE_COPY_FLUSH:
            /* nrOfTicks counts non-wide samples here. */
        silence_copy_flush:
            nrOfTicks = min((ctx->stop_holdoff_end -
                             ctx->stop_holdoff_offset),
                            (*osamp - nrOfOutSamplesWritten));
            nrOfTicks -= nrOfTicks % ctx->channels;
            if (nrOfTicks > 0) {
                memcpy(output, &ctx->stop_holdoff[ctx->stop_holdoff_offset],
                       nrOfTicks * sizeof(sox_sample_t));
                ctx->stop_holdoff_offset += nrOfTicks;
                output += nrOfTicks;
                nrOfOutSamplesWritten += nrOfTicks;
            }
            /* If fully drained holdoff then return to copy mode */
            if (ctx->stop_holdoff_offset == ctx->stop_holdoff_end) {
                ctx->stop_holdoff_offset = 0;
                ctx->stop_holdoff_end = 0;
                ctx->mode = SILENCE_COPY;
                goto silence_copy;
            }
            break;

        case SILENCE_STOP:
        silence_stop:
            break;
    }

    *isamp = nrOfInSamplesRead;
    *osamp = nrOfOutSamplesWritten;

    return 1;


}

static int sox_silence_drain(SilenceCtx *ctx, sox_sample_t *obuf, int *osamp) {
    int i;
    int nrOfTicks, nrOfOutSamplesWritten = 0; /* non-wide samples */

    /* Only if in flush mode will there be possible samples to write
     * out during drain() call.
     */
    if (ctx->mode == SILENCE_COPY_FLUSH ||
        ctx->mode == SILENCE_COPY) {
        nrOfTicks = min((ctx->stop_holdoff_end -
                         ctx->stop_holdoff_offset), *osamp);
        nrOfTicks -= nrOfTicks % ctx->channels;
        for (i = 0; i < nrOfTicks; i++) {
            *obuf++ = ctx->stop_holdoff[ctx->stop_holdoff_offset++];
            nrOfOutSamplesWritten++;
        }

        /* If fully drained holdoff then stop */
        if (ctx->stop_holdoff_offset == ctx->stop_holdoff_end) {
            ctx->stop_holdoff_offset = 0;
            ctx->stop_holdoff_end = 0;
            ctx->mode = SILENCE_STOP;
        }
    }

    *osamp = nrOfOutSamplesWritten;
    if (ctx->mode == SILENCE_STOP || *osamp == 0)
        return 0;
    else
        return 1;
}

static int sox_silence_free(SilenceCtx *ctx) {
    if (ctx) {
        free(ctx->window);
        free(ctx->start_holdoff);
        free(ctx->stop_holdoff);
        free(ctx);
    }
    return 1;
}


static SilenceCtx *initSilenceCtx(uint32_t sampleRate, uint32_t channels) {
    SilenceCtx *ctx = (SilenceCtx *) calloc(sizeof(SilenceCtx), 1);
    if (ctx == NULL)
        return NULL;
    ctx->rate = sampleRate;
    ctx->channels = channels;
    return ctx;
}

static void
silenceRemoveFilter(SilenceCtx *ctx, uint64_t sampleCount, const sox_sample_t *input, sox_sample_t *output,
                    size_t *out_size) {
    int out_count = sampleCount;
    int in_count = sampleCount;
    sox_silence_flow(ctx, input, output, &in_count, &out_count);
    *out_size = out_count;
}

// silence: usage: [ -l ] above_periods [ duration threshold[d|%] ] [ below_periods duration threshold[d|%] ]
// silence 1 0.1 1% -1 0.1 1%
// leave_silence  start_periods  start_duration start_threshold stop_periods stop_duration stop_threshold
static size_t
silenceRemove(uint32_t sampleRate, uint64_t totalSampleCount, uint32_t channels, sox_sample_t *data_in,
              sox_sample_t *data_out,
              bool leave_silence,
              int start_periods,
              float start_duration_val,
              float start_threshold,
              int stop_periods,
              float stop_duration_val,
              float stop_threshold) {
    size_t out_len = 0;
    SilenceCtx *silenceRemoveContext = initSilenceCtx(sampleRate, channels);

    char start_unit = '%';
    char stop_unit = '%';
    sox_silence_config(silenceRemoveContext, leave_silence, start_periods, stop_periods,
                       start_duration_val, stop_duration_val, stop_threshold, start_threshold, start_unit,
                       stop_unit
    );
    int ret = sox_silence_start(silenceRemoveContext);
    if (ret != -1) {
        silenceRemoveFilter(silenceRemoveContext, totalSampleCount, data_in, data_out, &out_len);
        sox_silence_free(silenceRemoveContext);
    }
    return out_len;
}