package com.gdn.partners.pcu.external.service.impl;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.pcu.external.client.feign.VideoFeign;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.VideoService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.VideoHashingRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoSignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoConfigurationRequest;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

@Slf4j
@Service
public class VideoServiceImpl implements VideoService {

  @Autowired
  private VideoFeign videoFeign;

  @Override
  public VideoSignedUrlResponse generateSignedUrl(VideoSignedUrlRequest videoSignedUrlRequest) {
    log.info("Generating signed url for video request : {} ", videoSignedUrlRequest);
    GdnPreconditions.checkArgument(Objects.nonNull(videoSignedUrlRequest),
        ErrorMessages.VIDEO_SIGNED_URL_REQUEST_NOT_EMPTY);
    Response<VideoSignedUrlResponse> videoUploadResponseResponse = videoFeign.generateSignedUrl(videoSignedUrlRequest);
    ResponseHelper.validateVideoResponse(videoUploadResponseResponse);
    return ResponseHelper.toVideoSignedUrlResponse(videoUploadResponseResponse.getData());
  }

  @Override
  public Map<String, Object> fetchVideoConfiguration(VideoConfigurationRequest request) {
    log.info("Fetching video configuration for request: {}", request);
    GdnPreconditions.checkArgument(Objects.nonNull(request),
        ErrorMessages.VIDEO_CONFIGURATION_REQUEST_NOT_EMPTY);
    Response<Map<String, Object>> response = videoFeign.fetchConfiguration(request);
    ResponseHelper.validateVideoResponse(response);
    return response.getData();
  }

  @Override
  public void retryVideoCompression(String videoId) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(videoId),
      ErrorMessages.VIDEO_ID_CANNOT_BE_EMPTY);
    videoFeign.retryVideoCompression(videoId);
  }

  @Override
  public void generateFingerPrint(String videoId) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(videoId),
      ErrorMessages.VIDEO_ID_CANNOT_BE_EMPTY);
    VideoHashingRequest request = VideoHashingRequest.builder().videoId(videoId).build();
    videoFeign.generateFingerPrint(request);
  }
}
