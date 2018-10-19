#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "timing.h"

#ifdef _WIN32

#include <Windows.h>

#else
#include <unistd.h>
#endif

//ref:  https://github.com/mackron/dr_libs/blob/master/dr_wav.h
#define DR_WAV_IMPLEMENTATION

#include "dr_wav.h"

#ifndef nullptr
#define nullptr 0
#endif

#ifndef MIN
#define MIN(A, B)        ((A) < (B) ? (A) : (B))
#endif

#ifndef MAX
#define MAX(A, B)        ((A) > (B) ? (A) : (B))
#endif

void wavWrite_f32(char *filename, float *buffer, size_t sampleRate, size_t totalSampleCount) {
    drwav_data_format format;
    format.container = drwav_container_riff;     // <-- drwav_container_riff = normal WAV files, drwav_container_w64 = Sony Wave64.
    format.channels = 1;
    format.sampleRate = (drwav_uint32) sampleRate;
    format.bitsPerSample = sizeof(float) * 8;
    format.format = DR_WAVE_FORMAT_IEEE_FLOAT;

    drwav *pWav = drwav_open_file_write(filename, &format);
    if (pWav) {
        drwav_uint64 samplesWritten = drwav_write(pWav, totalSampleCount, buffer);
        drwav_uninit(pWav);
        if (samplesWritten != totalSampleCount) {
            fprintf(stderr, "ERROR\n");
            exit(1);
        }
    }
}

float *wavRead_f32(char *filename, uint32_t *sampleRate, uint64_t *totalSampleCount) {
    unsigned int channels;
    float *buffer = drwav_open_and_read_file_f32(filename, &channels, sampleRate,
                                                 totalSampleCount);
    if (buffer == nullptr) {
        fprintf(stderr, "ERROR\n");
        exit(1);
    }
    //only channels
    if (channels != 1) {
        drwav_free(buffer);
        buffer = nullptr;
        *sampleRate = 0;
        *totalSampleCount = 0;
    }
    return buffer;
}

void splitpath(const char *path, char *drv, char *dir, char *name, char *ext) {
    const char *end;
    const char *p;
    const char *s;
    if (path[0] && path[1] == ':') {
        if (drv) {
            *drv++ = *path++;
            *drv++ = *path++;
            *drv = '\0';
        }
    } else if (drv)
        *drv = '\0';
    for (end = path; *end && *end != ':';)
        end++;
    for (p = end; p > path && *--p != '\\' && *p != '/';)
        if (*p == '.') {
            end = p;
            break;
        }
    if (ext)
        for (s = end; (*ext = *s++);)
            ext++;
    for (p = end; p > path;)
        if (*--p == '\\' || *p == '/') {
            p++;
            break;
        }
    if (name) {
        for (s = p; s < end;)
            *name++ = *s++;
        *name = '\0';
    }
    if (dir) {
        for (s = path; s < p;)
            *dir++ = *s++;
        *dir = '\0';
    }
}

#include "silence.c"

void silence(char *in_file, char *out_file) {
    uint32_t sampleRate = 0;
    uint64_t nSampleCount = 0;
    int channels = 1;
    float *data_in = wavRead_f32(in_file, &sampleRate, &nSampleCount);
    if (data_in != NULL) {
        double startTime = now();
        float *data_out = (float *) calloc(nSampleCount, sizeof(float));
        if (data_out != NULL) {
            bool leave_silence = false;
            int start_periods = 1;
            float start_duration_val = 0.1f;
            float start_threshold = 1;
            int stop_periods = -1;
            float stop_duration_val = 0.1f;
            float stop_threshold = 1;
            nSampleCount = silenceRemove(sampleRate, nSampleCount, channels, data_in, data_out,
                                         leave_silence,
                                         start_periods,
                                         start_duration_val,
                                         start_threshold,
                                         stop_periods,
                                         stop_duration_val,
                                         stop_threshold);
            double time_interval = calcElapsed(startTime, now());
            printf("time interval: %d ms\n ", (int) (time_interval * 1000));
            wavWrite_f32(out_file, data_out, sampleRate, (uint32_t) nSampleCount);
            free(data_out);
        }
        free(data_in);
    }
}

int main(int argc, char *argv[]) {
    printf("Silence Remove Effect Port From SoX\n");
    printf("blog:http://cpuimage.cnblogs.com/\n");

    if (argc < 2)
        return -1;
    char *in_file = argv[1];
    char drive[3];
    char dir[256];
    char fname[256];
    char ext[256];
    char out_file[1024];
    splitpath(in_file, drive, dir, fname, ext);
    sprintf(out_file, "%s%s%s_out%s", drive, dir, fname, ext);
    silence(in_file, out_file);

    printf("press any key to exit.\n");
    getchar();
    return 0;
}

