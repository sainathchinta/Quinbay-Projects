package com.gdn.mta.product.repository;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.x.businesspartner.commons.enums.MerchantStatus;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.entity.Company;
import com.gdn.x.businesspartner.entity.Profile;
import com.gdn.x.businesspartner.entity.ResponsiblePerson;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;

@Repository
public class BusinessPartnerRepositoryBean implements BusinessPartnerRepository {
  
  private static final Logger LOGGER = LoggerFactory
      .getLogger(BusinessPartnerRepositoryBean.class);

  @Value("${pickup.point.filter.batch.size}")
  private int pickupPointFilterBatchSize;

  @Autowired
  private XbpFeign xbpFeign;

  public XbpFeign getXbpFeign() { return xbpFeign; }

  public void setXbpFeign(XbpFeign xbpFeign) {
    this.xbpFeign = xbpFeign;
  }

  private Profile getProfileFromResponse(ProfileResponse response) {
    Profile businessPartner = null;
    ResponsiblePerson responsiblePerson = null;
    Company company = null;
    if (response != null) {
      businessPartner = new Profile();
      responsiblePerson = new ResponsiblePerson();
      company = new Company();
      BeanUtils.copyProperties(response, businessPartner);
      BeanUtils.copyProperties(response.getResponsiblePerson(), responsiblePerson);
      BeanUtils.copyProperties(response.getCompany(), company);
      businessPartner.setResponsiblePerson(responsiblePerson);
      businessPartner.setCompany(company);
      businessPartner.setMerchantStatus(MerchantStatus.valueOf(response.getMerchantStatus()));
    }
    return businessPartner;
  }

  @Override
  public ProfileResponse filterDetailByBusinessPartnerCode(String businessPartnerCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (StringUtils.isEmpty(storeId)) {
      storeId = Constants.DEFAULT_STORE_ID;
    }
    String requestId = GdnMandatoryRequestParameterUtil.getRequestId();
    if (StringUtils.isEmpty(requestId)) {
      requestId = Constants.DEFAULT_REQUEST_ID;
    }
    String userName = GdnMandatoryRequestParameterUtil.getUsername();
    if (StringUtils.isEmpty(userName)) {
      userName = Constants.DEFAULT_USERNAME;
    }
    GdnRestSingleResponse<ProfileResponse> response = this.getXbpFeign()
        .getBusinessPartnerDetails(businessPartnerCode, userName, false, false,
            storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, requestId);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<ProfileResponse> filterDetailsByBusinessPartnerCodeList(
      BusinessPartnerCodesRequest businessPartnerCodesRequest) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (StringUtils.isEmpty(storeId)) {
      storeId = Constants.DEFAULT_STORE_ID;
    }
    String requestId = GdnMandatoryRequestParameterUtil.getRequestId();
    if (StringUtils.isEmpty(requestId)) {
      requestId = Constants.DEFAULT_REQUEST_ID;
    }
    String userName = GdnMandatoryRequestParameterUtil.getUsername();
    if (StringUtils.isEmpty(userName)) {
      userName = Constants.DEFAULT_USERNAME;
    }
    BusinessPartnerFilterRequest businessPartnerFilterRequest = new BusinessPartnerFilterRequest();
    businessPartnerFilterRequest
        .setBusinessPartnerCodes(new HashSet<>(businessPartnerCodesRequest.getBusinessPartnerCodes()));
    GdnRestListResponse<ProfileResponse> response = this.getXbpFeign()
        .getBusinessPartnersDetails(userName, businessPartnerFilterRequest, 0,
            businessPartnerFilterRequest.getBusinessPartnerCodes().size(), storeId, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, requestId);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<PickupPointResponse> filterPickupPointsByPickupPointRequest(
      PickupPointFilterRequest pickupPointFilterRequest) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (StringUtils.isEmpty(storeId)) {
      storeId = Constants.DEFAULT_STORE_ID;
    }
    String requestId = GdnMandatoryRequestParameterUtil.getRequestId();
    if (StringUtils.isEmpty(requestId)) {
      requestId = Constants.DEFAULT_REQUEST_ID;
    }
    String userName = GdnMandatoryRequestParameterUtil.getUsername();
    if (StringUtils.isEmpty(userName)) {
      userName = Constants.DEFAULT_USERNAME;
    }
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    GdnRestListResponse<PickupPointResponse> response;
    int page = 0;
    do {
      response = this.getXbpFeign()
          .filter(userName, page, pickupPointFilterBatchSize, storeId, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, requestId, pickupPointFilterRequest);
      if (!response.isSuccess()) {
        throw new ApplicationException(ErrorCategory.UNSPECIFIED,
            "[" + response.getErrorCode() + "] " + response.getErrorMessage());
      }
      pickupPointResponseList.addAll(response.getContent());
      page++;
    } while ((long) page * pickupPointFilterBatchSize < response.getPageMetaData().getTotalRecords());

    return pickupPointResponseList;
  }

  @Override
  public List<PickupPointResponse> filterPickupPointsByPickupPointRequestForOne(
      PickupPointFilterRequest pickupPointFilterRequest) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (StringUtils.isEmpty(storeId)) {
      storeId = Constants.DEFAULT_STORE_ID;
    }
    String requestId = GdnMandatoryRequestParameterUtil.getRequestId();
    if (StringUtils.isEmpty(requestId)) {
      requestId = Constants.DEFAULT_REQUEST_ID;
    }
    String userName = GdnMandatoryRequestParameterUtil.getUsername();
    if (StringUtils.isEmpty(userName)) {
      userName = Constants.DEFAULT_USERNAME;
    }
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    GdnRestListResponse<PickupPointResponse> response;
    int page = 0;
    int size = 1;
    response = this.getXbpFeign()
        .filter(userName, page, size, storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, requestId,
            pickupPointFilterRequest);
    pickupPointResponseList.addAll(response.getContent());
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return pickupPointResponseList;
  }

  @Override
  public Profile filterByCode(String code) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (StringUtils.isEmpty(storeId)) {
      storeId = Constants.DEFAULT_STORE_ID;
    }
    String requestId = GdnMandatoryRequestParameterUtil.getRequestId();
    if (StringUtils.isEmpty(requestId)) {
      requestId = Constants.DEFAULT_REQUEST_ID;
    }
    String userName = GdnMandatoryRequestParameterUtil.getUsername();
    if (StringUtils.isEmpty(userName)) {
      userName = Constants.DEFAULT_USERNAME;
    }
    ProfileResponse response = getXbpFeign().getBusinessPartnerDetails(code, userName, false, false, storeId,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, requestId).getValue();
    return getProfileFromResponse(response);
  }

}
