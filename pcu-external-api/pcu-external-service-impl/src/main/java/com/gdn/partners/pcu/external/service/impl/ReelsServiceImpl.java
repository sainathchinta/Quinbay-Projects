package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.client.feign.ReelsFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.client.helper.ReelsResponse;
import com.gdn.partners.pcu.external.service.ReelsService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.ReelProductListingWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsCreationRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.ReelProductDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import com.gdn.partners.pcu.external.web.model.response.VideoDetailWebResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class ReelsServiceImpl implements ReelsService {

  @Autowired
  private ReelsFeign reelsFeign;

  @Autowired
  private XProductFeign xProductFeign;

  @Override
  public Page<ReelsListingResponse> getReelsListing(String businessPartnerCode, int page, int size,
      String sort) {
    ReelsResponse<List<ReelsListingResponse>> response =
        reelsFeign.getReelsListing(page, size, null, sort, businessPartnerCode);
    ResponseHelper.validateResponseForReels(response, true);
    return new PageImpl<>(response.getData(), PageRequest.of(page, size),
        response.getPaging().getTotalElements());
  }

  @Override
  public void updateReelsProducts(ReelsUpdateRequest reelsUpdateRequest,
      String businessPartnerCode) {
    reelsUpdateRequest.setProductSkuList(
        reelsUpdateRequest.getProductSkuList().stream().distinct().collect(Collectors.toList()));
    ReelsResponse<Void> sellerReelsResponse =
        reelsFeign.updateReelsProducts(businessPartnerCode, reelsUpdateRequest);
    ResponseHelper.validateResponseForReels(sellerReelsResponse, false);
  }

  @Override
  public void createNewReelsMapping(ReelsCreationRequest reelsCreationRequest) {
    ReelsResponse<Void> sellerReelsResponse =
      reelsFeign.createNewReelsMapping(reelsCreationRequest);
    ResponseHelper.validateResponseForReels(sellerReelsResponse, false);
  }

  @Override
  public VideoDetailWebResponse getVideoDetails(String reelId, String businessPartnerCode, int videoSource) {
    ReelsResponse<ReelsListingResponse> response =
        reelsFeign.getVideoDetails(reelId, businessPartnerCode, videoSource);
    ResponseHelper.validateResponseForReels(response, true);
    ReelProductListingWebRequest reelProductListingWebRequest = new ReelProductListingWebRequest();
    reelProductListingWebRequest.setProductSkuList(response.getData().getProductSkuList());
    GdnRestListResponse<ReelProductDetailWebResponse> xProductResponse =
        xProductFeign.getReelProductList(0, reelProductListingWebRequest.getProductSkuList().size(),
            reelProductListingWebRequest);
    ResponseHelper.validateResponse(xProductResponse);
    return ResponseHelper.toVideoDetailWebResponse(xProductResponse.getContent(),
        response.getData());
  }

  @Override
  public void deleteReels(String videoId, String businessPartnerCode) {
    ReelsResponse<Void> sellerReelsResponse = reelsFeign.deleteReels(videoId, businessPartnerCode);
    ResponseHelper.validateResponseForReels(sellerReelsResponse, false);
  }
}