package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.request.ReelsCreationRequest;
import com.gdn.partners.pcu.external.web.model.request.ReelsUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.ReelsListingResponse;
import com.gdn.partners.pcu.external.web.model.response.VideoDetailWebResponse;
import org.springframework.data.domain.Page;

public interface ReelsService {

  /**
   * Get paginated list of reels for a business partner
   *
   * @param businessPartnerCode the business partner code
   * @param page                the page number (0-based)
   * @param size                the page size
   * @param sortBy              the field to sort by (default: "createdDate")
   * @return Page containing list of reels
   */
  Page<ReelsListingResponse> getReelsListing(String businessPartnerCode, int page, int size,
      String sortBy);

  /**
   * Update seller reels products based on the provided videoId and product SKUs.
   *
   * @param reelsUpdateRequest
   * @param businessPartnerCode
   */
  void updateReelsProducts(ReelsUpdateRequest reelsUpdateRequest, String businessPartnerCode);

  /**
   * Adds a new reel based on the provided ReelsUpdateRequest.
   *
   * @param reelsCreationRequest The request object containing details for the new reel.
   */
  void createNewReelsMapping(ReelsCreationRequest reelsCreationRequest);

  VideoDetailWebResponse getVideoDetails(String reelId, String businessPartnerCode, int videoSource);

  /**
   * Deletes reels associated with a specific video ID.
   *
   * @param videoId             The unique identifier of the video whose associated reels are to be deleted.
   * @param businessPartnerCode The identifier of the business partner whose reels are to be deleted.
   */
  void deleteReels(String videoId, String businessPartnerCode);
}