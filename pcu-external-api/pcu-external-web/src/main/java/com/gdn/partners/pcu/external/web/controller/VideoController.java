package com.gdn.partners.pcu.external.web.controller;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.VideoApiPath;
import com.gdn.partners.pcu.external.service.VideoService;
import com.gdn.partners.pcu.external.web.model.request.VideoSignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoConfigurationRequest;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.Objects;

import static com.gdn.partners.pcu.external.model.Constants.BSC;
import static com.gdn.partners.pcu.external.model.Constants.ROOT;
import static com.gdn.partners.pcu.external.model.Constants.VIDEO;

@Slf4j
@Tag(name = "Video API")
@RestController
@RequestMapping(value = VideoApiPath.BASE_PATH)
public class VideoController {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private VideoService videoService;

  @Operation(summary = "Generate Signed URL", description = "Generate Signed URL")
  @PostMapping(value = VideoApiPath.GENERATE_SIGNED_URL)
  public SingleBaseResponse<VideoSignedUrlResponse> generateSignedUrl(
      @RequestBody VideoSignedUrlRequest videoSignedUrlRequest) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    Boolean videoActivated = mandatoryParameterHelper.getIsProductVideoActivated();
    if(Boolean.FALSE.equals(videoActivated) && VIDEO.equals(videoSignedUrlRequest.getSource()) && BSC.equals(videoSignedUrlRequest.getClientId())) {
      throw new ValidationException(ErrorCategory.AUTHORIZATION.getCode());
    }
    String videoName = videoSignedUrlRequest.getVideoName();
    if (Objects.nonNull(videoName)) {
      videoSignedUrlRequest.setVideoName(videoName.replace(ROOT, StringUtils.EMPTY));
    }
    videoSignedUrlRequest.setOwnerCode(businessPartnerCode);
    VideoSignedUrlResponse videoSignedUrlResponse =
        videoService.generateSignedUrl(videoSignedUrlRequest);
    return new SingleBaseResponse<>(null, null, true, requestId, videoSignedUrlResponse);
  }

  @Operation(summary = "Fetch Video Configuration", description = "Fetch video upload "
      + "configuration based on client and source type")
  @PostMapping(value = VideoApiPath.FETCH_CONFIGURATION)
  public SingleBaseResponse<Map<String, Object>> getVideoConfiguration(
      @RequestBody VideoConfigurationRequest videoConfigurationRequest) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    Map<String, Object> configuration =
        videoService.fetchVideoConfiguration(videoConfigurationRequest);
    return new SingleBaseResponse<>(null, null, true, requestId, configuration);
  }
}
