package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.List;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.service.PickupPointService;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.PickupPointModel;
import com.gdn.mta.bulk.models.download.responsedata.StoreCopyUploadTemplateResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.x.businesspartner.dto.ProfileResponse;

import lombok.extern.slf4j.Slf4j;

@Service(value = "bulkStoreCopyUploadTemplateServiceBean")
@Slf4j
public class BulkStoreCopyUploadTemplateServiceBean implements BulkProcessDataService {

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private PickupPointService pickupPointService;

  @Value("${bp.bopis.restriction.enabled}")
  private boolean bpBopisRestrictionEnabled;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    ProfileResponse businessPartner =
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
          request.getMerchantId());
    ShippingTypeEligibility shippingTypeEligibility =
      ConverterUtil.getShippingEligibility(businessPartner, bpBopisRestrictionEnabled);
    return new StoreCopyUploadTemplateResponse(getPickupPoints(businessPartner), shippingTypeEligibility);
  }

  private List<PickupPointModel> getPickupPoints(ProfileResponse businessPartner)
    throws ApplicationException {
    List<PickupPointResponse> pickupPointResponseList = this.pickupPointService
      .getPickupPointSummaryFilter(0, PickupPointFilterRequest.builder()
          .businessPartnerCode(businessPartner.getBusinessPartnerCode()).build());
    List<PickupPointModel> pickupPointModelList = new ArrayList<>();
    for (PickupPointResponse pickupPointResponse : pickupPointResponseList) {
      pickupPointModelList
        .add(new PickupPointModel(pickupPointResponse.getName(), pickupPointResponse.getCode(), pickupPointResponse.isFbbActivated()));
    }
    return pickupPointModelList;
  }
}
