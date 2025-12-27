package com.gdn.partners.pcu.external.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.client.feign.VideoFeign;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.VideoConfigurationRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoHashingRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoSignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;

import java.util.Collections;
import java.util.Map;

class VideoServiceImplTest {

    @Mock
    private VideoFeign videoFeign;

    @InjectMocks
    private VideoServiceImpl videoService;

    private VideoSignedUrlRequest request;
    private VideoSignedUrlResponse expectedResponse;
    private Response<VideoSignedUrlResponse> feignResponse;

    private VideoConfigurationRequest configRequest;
    private Map<String, Object> configMap;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        request = new VideoSignedUrlRequest();
        request.setClientId("test-client");
        request.setSource("test-owner-type");
        request.setOwnerCode("test-owner-code");
        request.setVideoName("test-video.mp4");
        request.setCompression(true);
        expectedResponse = new VideoSignedUrlResponse();
        expectedResponse.setVideoSignedUrl("https://test-signed-url.com/video");
        expectedResponse.setVideoId("test-video-id");
        expectedResponse.setVideoUploadUrl("https://test-upload-url.com/video");
        expectedResponse.setImageSignedUrl("https://test-signed-url.com/image");
        expectedResponse.setImageUploadUrl("https://test-upload-url.com/image");
        expectedResponse.setExpiresAt(System.currentTimeMillis() + 3600000);
        feignResponse = new Response<>(
            HttpStatus.OK.value(),
            HttpStatus.OK.name(),
            expectedResponse,
            null,
            null
        );
        configRequest = new VideoConfigurationRequest();
        configRequest.setClientId("test-client");
        configRequest.setSource("UPLOAD");

        configMap =
            Map.of("minimumDuration", 10, "maximumDuration", 90, "maximumFileSize", 30, "format",
                "video/mp4", "height", 1280, "width", 1280);
    }

    @AfterEach
    public void tearDown() {
        Mockito.verifyNoMoreInteractions(videoFeign);
    }

    @Test
    void generateSignedUrl_Success() throws Exception {
        feignResponse.setStatus("SUCCESS");
        when(videoFeign.generateSignedUrl(any(VideoSignedUrlRequest.class))).thenReturn(feignResponse);
        VideoSignedUrlResponse actualResponse = videoService.generateSignedUrl(request);
        Mockito.verify(videoFeign).generateSignedUrl(any(VideoSignedUrlRequest.class));
        assertEquals(expectedResponse.getVideoSignedUrl(), actualResponse.getVideoSignedUrl());
        assertEquals(expectedResponse.getVideoId(), actualResponse.getVideoId());
        assertEquals(expectedResponse.getVideoUploadUrl(), actualResponse.getVideoUploadUrl());
        assertEquals(expectedResponse.getImageSignedUrl(), actualResponse.getImageSignedUrl());
        assertEquals(expectedResponse.getImageUploadUrl(), actualResponse.getImageUploadUrl());
        assertEquals(expectedResponse.getExpiresAt(), actualResponse.getExpiresAt());
    }

    @Test
    void generateSignedUrl_NullRequest() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            videoService.generateSignedUrl(null);
        }, ErrorMessages.VIDEO_SIGNED_URL_REQUEST_NOT_EMPTY);
    }

    @Test
    void generateSignedUrl_FeignError() {
        Response<VideoSignedUrlResponse> errorResponse = new Response<>(
            HttpStatus.BAD_REQUEST.value(),
            HttpStatus.BAD_REQUEST.name(),
            null,
            Collections.singletonMap("error", Collections.singletonList("Error message")),
            null
        );
        when(videoFeign.generateSignedUrl(any(VideoSignedUrlRequest.class))).thenReturn(errorResponse);
        assertThrows(RuntimeException.class, () -> {
            videoService.generateSignedUrl(request);
        });
        Mockito.verify(videoFeign).generateSignedUrl(any(VideoSignedUrlRequest.class));
    }

    @Test
    void generateSignedUrl_FeignThrowsException() {
        when(videoFeign.generateSignedUrl(any(VideoSignedUrlRequest.class)))
            .thenThrow(new RuntimeException("Feign client error"));
        assertThrows(RuntimeException.class, () -> {
            videoService.generateSignedUrl(request);
        });
        verify(videoFeign).generateSignedUrl(any(VideoSignedUrlRequest.class));
    }

    @Test
    void fetchVideoConfiguration_WhenRequestIsNull() {
        Exception exception = assertThrows(ApplicationRuntimeException.class, () -> {
            videoService.fetchVideoConfiguration(null);
        });
            assertEquals(
            "Can not process invalid input data :Video Configuration request cannot be null",
            exception.getMessage());
        verify(videoFeign, Mockito.never()).fetchConfiguration(
            any(VideoConfigurationRequest.class));
    }

    @Test
    void fetchVideoConfiguration_Success() throws Exception {
        Map<String, Object> configMap =
            Map.of("minimumDuration", 10, "maximumDuration", 90, "maximumFileSize", 30, "format",
                "video/mp4", "height", 1280, "width", 1280);
        Response<Map<String, Object>> feignResponse =
            new Response<>(200, "SUCCESS", configMap, null, null);
        when(videoFeign.fetchConfiguration(any(VideoConfigurationRequest.class))).thenReturn(
            feignResponse);
        Map<String, Object> response = videoService.fetchVideoConfiguration(configRequest);
        verify(videoFeign).fetchConfiguration(configRequest);
        assertNotNull(response);
        assertEquals(10, response.get("minimumDuration"));
        assertEquals(90, response.get("maximumDuration"));
        assertEquals(30, response.get("maximumFileSize"));
        assertEquals("video/mp4", response.get("format"));
        assertEquals(1280, response.get("height"));
        assertEquals(1280, response.get("width"));
    }

    @Test
    void fetchVideoConfiguration_WhenFeignReturnsError() {
        Response<Map<String, Object>> errorResponse =
            new Response<>(400, "ERROR", null,
                Collections.singletonMap("error",
                    Collections.singletonList("Configuration not found")), null);
        when(videoFeign.fetchConfiguration(any(VideoConfigurationRequest.class))).thenReturn(
            errorResponse);

        Exception exception = assertThrows(RuntimeException.class, () -> {
            videoService.fetchVideoConfiguration(configRequest);
        });

        verify(videoFeign).fetchConfiguration(configRequest);
        assertEquals("Configuration not found", exception.getMessage());
    }

    @Test
    void fetchVideoConfiguration_WhenFeignThrowsException() {
        when(videoFeign.fetchConfiguration(any(VideoConfigurationRequest.class))).thenThrow(
            new RuntimeException("Service unavailable"));

        Exception exception = assertThrows(RuntimeException.class, () -> {
            videoService.fetchVideoConfiguration(configRequest);
        });

        verify(videoFeign).fetchConfiguration(configRequest);
        assertEquals("Service unavailable", exception.getMessage());
    }

    @Test
    void retryVideoCompression_EmptyVideoId() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            videoService.retryVideoCompression("");
        }, ErrorMessages.VIDEO_ID_CANNOT_BE_EMPTY);
    }

    @Test
    void retryVideoCompression_NullVideoId() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            videoService.retryVideoCompression(null);
        }, ErrorMessages.VIDEO_ID_CANNOT_BE_EMPTY);
    }

    @Test
    void retryVideoCompression_Success() throws Exception {
        String videoId = "test-video-id";
        videoService.retryVideoCompression(videoId);
        verify(videoFeign).retryVideoCompression(videoId);
    }

    @Test
    void generateFingerPrint_Success() throws Exception {
        String videoId = "test-video-id";
        Response<Void> feignResponse =
          new Response<>(HttpStatus.OK.value(), "SUCCESS", null, null, null);
        when(videoFeign.generateFingerPrint(any(VideoHashingRequest.class))).thenReturn(
          feignResponse);
        videoService.generateFingerPrint(videoId);
        verify(videoFeign).generateFingerPrint(any(VideoHashingRequest.class));
    }
}