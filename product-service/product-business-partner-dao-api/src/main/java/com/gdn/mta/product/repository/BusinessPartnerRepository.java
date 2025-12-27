package com.gdn.mta.product.repository;

import java.util.List;

import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.entity.Profile;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;

public interface BusinessPartnerRepository {

  List<PickupPointResponse> filterPickupPointsByPickupPointRequest(PickupPointFilterRequest pickupPointFilterRequest)
      throws Exception;

  List<PickupPointResponse> filterPickupPointsByPickupPointRequestForOne(
      PickupPointFilterRequest pickupPointFilterRequest) throws Exception;

  Profile filterByCode(String code) throws  Exception;

  ProfileResponse filterDetailByBusinessPartnerCode(String businessPartnerCode) throws Exception;

  List<ProfileResponse> filterDetailsByBusinessPartnerCodeList(
      BusinessPartnerCodesRequest businessPartnerCodesRequest) throws Exception;
}
