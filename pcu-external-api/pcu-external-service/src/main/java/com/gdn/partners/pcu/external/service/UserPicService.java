package com.gdn.partners.pcu.external.service;

import com.gdn.x.businesspartner.dto.ProfileResponse;

import java.util.Set;

public interface UserPicService {

  /**
   * Validates user-pic pickup points based on the provided set of pickup point codes.
   *
   * @param pickupPointCodesInRequest A set of pickup point codes to be validated.
   * @throws Exception If there is an error during the validation process.
   */

  void validateUserPicPickupPoints(Set<String> pickupPointCodesInRequest) throws Exception;

  /**
   * Validates user pic pickup points for a specific business partner based on the provided set
   * of pickup point codes
   * and business partner profile.
   *
   * @param pickupPointCodesInRequest A set of pickup point codes to be validated.
   * @param businessPartnerProfile    The business partner's profile for validation.
   * @throws Exception If there is an error during the validation process.
   */
  void validateUserPicPickupPointsForBusinessPartner(Set<String> pickupPointCodesInRequest,
      ProfileResponse businessPartnerProfile) throws Exception;

  /**
   * Checks if the MPP (Multiple Pickup Points) is enabled for the given profile response and
   * allowed sellers.
   *
   * @param profileResponse   The profile response to check for MPP enablement.
   * @return True if MPP is enabled, false otherwise.
   */
  boolean isMppEnabled(ProfileResponse profileResponse);

  /**
   *
   * @param businessPartnerProfile
   * @return
   */
  boolean shouldRestrictAccess(ProfileResponse businessPartnerProfile);
  /**
   * Returns set of ppcodes, and filters out user inaccessible ppcodes if applicable
   *
   * @param businessPartnerProfile will be fetched internally if null is passed
   * @param pickupPointCodesInRequest must not be null, can be empty
   * @return
   */
  Set<String> filterInaccessiblePickupPoints(ProfileResponse businessPartnerProfile,
    Set<String> pickupPointCodesInRequest);

  /**
   * check userpic switch, mppEnabled to return accessible ppCodes
   *
   * @param businessPartnerProfile must not be null
   * @return
   */
  Set<String> fetchAccessiblePickupPointCodes(ProfileResponse businessPartnerProfile);
}
