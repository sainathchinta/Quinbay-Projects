package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.client.feign.ReelsFeign;
import com.gdn.partners.pcu.external.client.helper.ReelsResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.ReelsCreationRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;

@Component
public class ReelsFallback implements ReelsFeign {

  @Override
  public ReelsResponse<List<ReelsListingResponse>> getReelsListing(int page, int size,
      String orderBy, String sortOrder, String ownerId) {
    return ReelsResponse.<List<ReelsListingResponse>>builder().status(HttpStatus.BAD_REQUEST.name())
        .data(Collections.emptyList()).errors(
            Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
                Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)))
        .metadata(Collections.emptyMap()).paging(null).build();
  }

  @Override
  public ReelsResponse<Void> updateReelsProducts(String businessPartnerCode,
      ReelsUpdateRequest reelsUpdateRequest) {
    return ReelsResponse.<Void>builder().status(HttpStatus.BAD_REQUEST.name()).errors(
            Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.name(),
                Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE))).data(null)
        .metadata(Collections.emptyMap()).build();
  }

  @Override
  public ReelsResponse<Void> createNewReelsMapping(ReelsCreationRequest reelsCreationRequest) {
    return ReelsResponse.<Void>builder().status(HttpStatus.BAD_REQUEST.name()).errors(
        Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.name(),
          Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE))).data(null)
      .metadata(Collections.emptyMap()).build();
  }


  @Override
  public ReelsResponse<ReelsListingResponse> getVideoDetails(String reelId, String ownerId,
      int videoSource) {
    return ReelsResponse.<ReelsListingResponse>builder().status(HttpStatus.BAD_REQUEST.name())
        .data(new ReelsListingResponse()).errors(
            Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
                Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)))
        .metadata(Collections.emptyMap()).paging(null).build();
  }

  @Override
  public ReelsResponse<Void> deleteReels(String videoId, String businessPartnerCode) {
    return ReelsResponse.<Void>builder().status(HttpStatus.BAD_REQUEST.name()).errors(
        Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.name(),
          Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE))).data(null)
      .metadata(Collections.emptyMap()).build();
  }
}