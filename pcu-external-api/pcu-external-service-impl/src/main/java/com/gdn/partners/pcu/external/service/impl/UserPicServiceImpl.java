package com.gdn.partners.pcu.external.service.impl;


import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Slf4j
@Service
public class UserPicServiceImpl implements UserPicService {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Value("${mpp.allowed.merchant-types}")
  private String mppAllowedMerchantTypes;

  @Value("${user.pic.feature.switch}")
  private boolean userPicFeatureSwitch;

  @Value("${user.pic.filter.threshold}")
  private int filterThreshold;

  @Value("${user.pic.channelId.excluded}")
  private Set<String> userPicExcludedChannelIds;


  @Override
  public void validateUserPicPickupPoints(Set<String> pickupPointCodesInRequest) throws Exception {
    String requestBusinessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    ProfileResponse businessPartnerProfile =
        businessPartnerService.filterByBusinessPartnerCode(requestBusinessPartnerCode);
    validateUserPicPickupPointsForBusinessPartner(pickupPointCodesInRequest,
        businessPartnerProfile);
  }

  @Override
  public void validateUserPicPickupPointsForBusinessPartner(Set<String> pickupPointCodesInRequest,
      ProfileResponse businessPartnerProfile) throws Exception {
    Set<String> pickupPointCodesInSession = mandatoryParameterHelper.getPickupPoints();
    if (!shouldRestrictAccess(businessPartnerProfile) ||
      CollectionUtils.isEmpty(pickupPointCodesInSession)) {
      return;
    }
    for (String requestedPickupPoint : pickupPointCodesInRequest) {
      if (!pickupPointCodesInSession.contains(requestedPickupPoint)) {
        log.error(
          "Error accessing a Pickup Point for user, PickupPoint: {} PartnerProfile: {} Exception: {} ",
          requestedPickupPoint, businessPartnerProfile, ErrorMessages.NO_ACCESS_TO_PICKUP_POINT);
        throw new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT);
      }
    }
  }

  @Override
  public boolean isMppEnabled(ProfileResponse profileResponse) {
    if (Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany).isEmpty()) {
      return false;
    }
    return Boolean.TRUE.equals(profileResponse.getCompany().isCncActivated()) ||
      (Boolean.TRUE.equals(profileResponse.getMultiDefaultAddressFlag()) &&
        mppAllowedMerchantTypes.contains(profileResponse.getCompany().getMerchantType()));
  }

  @Override
  public boolean shouldRestrictAccess(ProfileResponse businessPartnerProfile) {
    String requestBusinessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();

    if (Objects.isNull(businessPartnerProfile)) {
      businessPartnerProfile = businessPartnerService.filterByBusinessPartnerCode(
        requestBusinessPartnerCode);
    }
    GdnPreconditions.checkArgument(Objects.nonNull(businessPartnerProfile),
      ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + requestBusinessPartnerCode);
    String requestChannelId = mandatoryParameterHelper.getChannelId();
    return userPicFeatureSwitch && isMppEnabled(businessPartnerProfile) &&
      !userPicExcludedChannelIds.contains(requestChannelId);
  }

  @Override
  public Set<String> filterInaccessiblePickupPoints(ProfileResponse businessPartnerProfile,
    Set<String> pickupPointCodesFromRequest) {
    Set<String> pickupPointCodesFromSession = mandatoryParameterHelper.getPickupPoints();
    if (!shouldRestrictAccess(businessPartnerProfile) ||
      CollectionUtils.isEmpty(pickupPointCodesFromSession)) {
      return pickupPointCodesFromRequest;
    }
    if (CollectionUtils.isEmpty(pickupPointCodesFromRequest)) {
      return pickupPointCodesFromRequest;
    }
    Set<String> filteredPickupPoints = new HashSet<>(pickupPointCodesFromRequest);
    filteredPickupPoints.retainAll(pickupPointCodesFromSession);
    if (filteredPickupPoints.size() <= filterThreshold) {
      return filteredPickupPoints;
    } else {
      return Collections.emptySet();
    }
  }

  @Override
  public Set<String> fetchAccessiblePickupPointCodes(ProfileResponse profileResponse) {
    if (!shouldRestrictAccess(profileResponse)) {
      return new HashSet<>();
    }
    return mandatoryParameterHelper.getPickupPoints();
  }
}
