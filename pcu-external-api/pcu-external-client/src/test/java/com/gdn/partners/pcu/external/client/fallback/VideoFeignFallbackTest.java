package com.gdn.partners.pcu.external.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.VideoConfigurationRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoHashingRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoSignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

import java.util.Collections;
import java.util.Map;

class VideoFeignFallbackTest {

    private VideoFeignFallback videoFeignFallback;
    private VideoSignedUrlRequest request;
    private static final String VIDEO_ID = "test-video-id";

    @BeforeEach
    void setUp() {
        videoFeignFallback = new VideoFeignFallback();
        request = new VideoSignedUrlRequest();
        request.setClientId("test-client");
        request.setSource("test-owner-type");
        request.setOwnerCode("test-owner-code");
        request.setVideoName("test-video.mp4");
        request.setCompression(true);
    }

    @Test
    void generateSignedUrl_ReturnsFallbackResponse() {
        Response<VideoSignedUrlResponse> response = videoFeignFallback.generateSignedUrl(request);
        assertEquals(HttpStatus.BAD_REQUEST.value(), response.getCode());
        assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
        assertNull(response.getData());
        assertEquals(
            Collections.singletonMap(
                ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
                Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)
            ),
            response.getErrors()
        );
        assertNull(response.getMetadata());
    }

    @Test
    void generateSignedUrl_WithNullRequest_ReturnsFallbackResponse() {
        Response<VideoSignedUrlResponse> response = videoFeignFallback.generateSignedUrl(null);
        assertEquals(HttpStatus.BAD_REQUEST.value(), response.getCode());
        assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
        assertNull(response.getData());
        assertEquals(
            Collections.singletonMap(
                ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
                Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)
            ),
            response.getErrors()
        );
        assertNull(response.getMetadata());
    }

    @Test
    void fetchConfigurationTest() {
        Response<Map<String, Object>> response = videoFeignFallback.fetchConfiguration(new VideoConfigurationRequest());
        assertEquals(HttpStatus.BAD_REQUEST.value(), response.getCode());
        assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
        assertNull(response.getData());
        assertEquals(
            Collections.singletonMap(
                ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
                Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)
            ),
            response.getErrors()
        );
        assertNull(response.getMetadata());
    }

    @Test
    void retryVideoCompressionTest() {
        Response<Void> response = videoFeignFallback.retryVideoCompression(VIDEO_ID);
        assertEquals(HttpStatus.BAD_REQUEST.value(), response.getCode());
        assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
        assertNull(response.getData());
        assertEquals(Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
          Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)), response.getErrors());
        assertNull(response.getMetadata());
    }

    @Test
    void generateFingerPrintTest() {
        Response<Void> response = videoFeignFallback.generateFingerPrint(
          VideoHashingRequest.builder().videoId(VIDEO_ID).build());
        assertEquals(HttpStatus.BAD_REQUEST.value(), response.getCode());
        assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
        assertNull(response.getData());
        assertEquals(Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
          Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)), response.getErrors());
        assertNull(response.getMetadata());
    }
}