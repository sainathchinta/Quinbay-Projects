package com.gdn.partners.pcu.external.client.fallback;

import java.util.Collections;

import com.gdn.partners.pcu.external.client.feign.VideoFeign;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.web.model.request.VideoSignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoConfigurationRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoHashingRequest;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.common.enums.ErrorCategory;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class VideoFeignFallback implements VideoFeign {
  @Override
  public Response<VideoSignedUrlResponse> generateSignedUrl(VideoSignedUrlRequest request) {
    return new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(), null,
        Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
            Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)), null);
  }

  @Override
  public Response<Map<String, Object>> fetchConfiguration(VideoConfigurationRequest request) {
    return new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(), null,
        Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
            Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)), null);
  }

  @Override
  public Response<Void> retryVideoCompression(String videoId) {
    return new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(), null,
      Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)), null);
  }

  @Override
  public Response<Void> generateFingerPrint(VideoHashingRequest request) {
    return new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(), null,
      Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)), null);
  }
} 