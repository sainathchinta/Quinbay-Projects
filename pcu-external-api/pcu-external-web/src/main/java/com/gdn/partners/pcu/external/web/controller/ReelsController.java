package com.gdn.partners.pcu.external.web.controller;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.SellerReelsApiPath;
import com.gdn.partners.pcu.external.service.ReelsService;
import com.gdn.partners.pcu.external.web.model.request.ReelsCreationRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import com.gdn.partners.pcu.external.web.model.response.VideoDetailWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

@Slf4j
@RestController
@RequestMapping(SellerReelsApiPath.BASE_PATH)
@Tag(name = "Seller Reels API", description = "APIs for managing reels")
public class ReelsController {

  @Autowired
  private ReelsService reelsService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${reels.product.sku.update.max.limit}")
  private int reelsProductSkuUpdateMaxLimit;

  @Operation(summary = "Get reels listing", description = "Get paginated list of reels for a "
      + "business partner")
  @PostMapping(value = SellerReelsApiPath.LISTING, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ReelsListingResponse> getReelsListing(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "25") int size,
      @RequestParam(value = "sort", defaultValue = "desc") String sort) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        ErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_EMPTY);
    log.info("Getting reels listing for businessPartnerCode: {}, page: {}, size: {}, sort: {}, "
        + "request id : {} ", businessPartnerCode, page, size, sort, requestId);
    Page<ReelsListingResponse> reelsListingResponses =
        reelsService.getReelsListing(businessPartnerCode, page, size, sort);
    return new ListBaseResponse<>(null, null, true, requestId, reelsListingResponses.getContent(),
        new Metadata(page, size, reelsListingResponses.getTotalElements()));
  }

  @Operation(summary = "Update seller reels", description = "Update product SKUs mapped to a "
      + "reel")
  @PostMapping(value = SellerReelsApiPath.UPDATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateSellerReel(@RequestBody ReelsUpdateRequest reelsUpdateRequest) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(Objects.nonNull(reelsUpdateRequest),
        ErrorMessages.REELS_UPDATE_REQUEST_CANNOT_BE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(reelsUpdateRequest.getVideoId()),
        ErrorMessages.VIDEO_ID_CANNOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        ErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_EMPTY);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(reelsUpdateRequest.getProductSkuList())
            && reelsUpdateRequest.getProductSkuList().size() <= reelsProductSkuUpdateMaxLimit,
        String.format(ErrorMessages.PRODUCT_SKUS_CANNOT_BE_EMPTY_OR_EXCEED_LIMIT_IN_REELS,
            reelsProductSkuUpdateMaxLimit));
    log.info("Updating reels products for videoId: {}, productSkus: {}, request id : {}", reelsUpdateRequest.getVideoId(),
        reelsUpdateRequest.getProductSkuList(), requestId);
    reelsService.updateReelsProducts(reelsUpdateRequest, businessPartnerCode);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Create new reels mapping", description = "Create new reels mapping with videoId and product SKUs")
  @PostMapping(value = SellerReelsApiPath.ADD_REEL, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse createNewReelsMapping(
      @RequestBody ReelsCreationRequest reelsCreationRequest) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    reelsCreationRequest.setOwnerId(businessPartnerCode);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(reelsCreationRequest.getVideoId()),
        ErrorMessages.VIDEO_ID_CANNOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        ErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_EMPTY);
    GdnPreconditions.checkArgument(
        CollectionUtils.isNotEmpty(reelsCreationRequest.getProductSkuList())
            && reelsCreationRequest.getProductSkuList().size() <= reelsProductSkuUpdateMaxLimit,
        String.format(ErrorMessages.PRODUCT_SKUS_CANNOT_BE_EMPTY_OR_EXCEED_LIMIT_IN_REELS,
            reelsProductSkuUpdateMaxLimit));
    log.info("Add new reels mapping videoId: {}, productSkus: {} ",
        reelsCreationRequest.getVideoId(), reelsCreationRequest.getProductSkuList());
    reelsService.createNewReelsMapping(reelsCreationRequest);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Get Reel Details with videoId", description = "Get Reel Details with "
      + "reelId")
  @GetMapping(value = SellerReelsApiPath.DETAILS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<VideoDetailWebResponse> getVideoDetails(
      @RequestParam(value = "videoId") String reelId, @RequestParam String businessPartnerCode,
      @RequestParam(defaultValue = "0") int videoSource) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String businessPartnerCodeFromSession = validateVideoDetailRequest(reelId, businessPartnerCode);
    log.info("Getting reel details for reelId: {}, businessPartnerCode: {}, request id : {} ",
        reelId, businessPartnerCodeFromSession, requestId);
    return new SingleBaseResponse<>(null, null, true, requestId,
        reelsService.getVideoDetails(reelId, businessPartnerCodeFromSession, videoSource));
  }

  @Operation(summary = "Delete reels", description = "Delete reels mapping by videoId")
  @PostMapping(value = SellerReelsApiPath.DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse deleteReels(@PathVariable("videoId") String videoId) {
    String userName = mandatoryParameterHelper.getUsername();
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(videoId),
      ErrorMessages.VIDEO_ID_CANNOT_BE_EMPTY);
    log.info("Deleting reels mapping videoId: {}, user: {} ", videoId, userName);
    reelsService.deleteReels(videoId, mandatoryParameterHelper.getBusinessPartnerCode());
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  private String validateVideoDetailRequest(String reelId, String businessPartnerCode) {
    String businessPartnerCodeFromSession = mandatoryParameterHelper.getBusinessPartnerCode();
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        ErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(reelId),
        ErrorMessages.VIDEO_ID_CANNOT_BE_EMPTY);
    GdnPreconditions.checkArgument(
        StringUtils.equals(businessPartnerCodeFromSession, businessPartnerCode),
        ErrorMessages.VIDEO_ACCESSED_BY_DIFFERENT_SELLER);
    return businessPartnerCodeFromSession;
  }
}