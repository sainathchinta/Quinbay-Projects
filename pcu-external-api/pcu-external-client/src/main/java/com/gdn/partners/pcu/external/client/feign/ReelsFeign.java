package com.gdn.partners.pcu.external.client.feign;

import com.gdn.partners.pcu.external.client.factory.ReelsFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.helper.ReelsResponse;
import com.gdn.partners.pcu.external.web.model.request.ReelsCreationRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(name = "reelsFeign", url = "${service.seller.reels.endpoint}", fallbackFactory =
    ReelsFeignFallbackFactory.class)
public interface ReelsFeign {

  @PostMapping(value = "/api/v1/reels/listing")
  ReelsResponse<List<ReelsListingResponse>> getReelsListing(@RequestParam int page,
      @RequestParam int size, @RequestParam String orderBy, @RequestParam String sortOrder,
      @RequestParam String ownerId);

  @PostMapping(value = "/api/v1/reels/update")
  ReelsResponse<Void> updateReelsProducts(@RequestParam("ownerId") String businessPartnerCode,
      @RequestBody ReelsUpdateRequest reelsUpdateRequest);

  @PostMapping(value = "/api/v1/reels/add-reel")
  ReelsResponse<Void> createNewReelsMapping(@RequestBody ReelsCreationRequest reelsCreationRequest);

  @GetMapping(value = "/api/v1/reels/reel-details-by-reel-id-and-owner-id")
  ReelsResponse<ReelsListingResponse> getVideoDetails(@RequestParam String reelId,
      @RequestParam String ownerId, @RequestParam(defaultValue = "0") int videoSource);

  @PostMapping(value = "/api/v1/reels/{videoId}/delete")
  ReelsResponse<Void> deleteReels(@PathVariable("videoId") String videoId,
    @RequestParam("ownerId") String businessPartnerCode);
}