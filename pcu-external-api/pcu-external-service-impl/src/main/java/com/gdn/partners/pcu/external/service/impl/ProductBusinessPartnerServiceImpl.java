package com.gdn.partners.pcu.external.service.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ProductCopyRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductItemBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.PickupPointService;
import com.gdn.partners.pcu.external.service.ProductBusinessPartnerService;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.DefaultConfigurationAndPickupPointRequest;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerProfileWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductBusinessPartnerResponse;
import com.gdn.partners.pcu.external.web.model.response.PickupPointWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.enums.ProductType;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 10/12/2018 AD.
 */
@Slf4j
@Service
public class ProductBusinessPartnerServiceImpl implements ProductBusinessPartnerService {

  private static final String ACTIVE = "ACTIVE";

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Value("${bp.bopis.restriction.enabled}")
  private boolean bpBopisRestrictionEnabled;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  public SingleBaseResponse<CreateProductBusinessPartnerResponse> create(
      ProductBusinessPartnerServiceRequest productBusinessPartnerServiceRequest) throws Exception {
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    if (!accessibility.contains(Accessibilty.STORE_PRODUCT_PRODUCT_CREATION)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    RequestHelper.validateRequest(productBusinessPartnerServiceRequest);
    ProfileResponse bpProfile = businessPartnerService
        . filterByBusinessPartnerCode(productBusinessPartnerServiceRequest.getBusinessPartnerCode());
    GdnPreconditions.checkArgument(Objects.nonNull(bpProfile) && ACTIVE.equals(bpProfile.getMerchantStatus()),
        ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + productBusinessPartnerServiceRequest.getBusinessPartnerCode());
    setProductTypeByMerchantOriginId(bpProfile, productBusinessPartnerServiceRequest);
    pickupPointService.validateAndSaveDefaultPickupPoint(productBusinessPartnerServiceRequest);
    GdnBaseRestResponse response =
        pbpFeign.create(RequestHelper.toProductBusinessPartnerRequest(productBusinessPartnerServiceRequest));
    ResponseHelper.validateResponse(response);
    CreateProductBusinessPartnerResponse createResponse =
        new CreateProductBusinessPartnerResponse();
    if ("ERR-PBP400008".equals(response.getErrorCode())) {
      createResponse.setLogisticsSaveSuccess(false);
    }
    return new SingleBaseResponse<>(response.getErrorMessage(), response.getErrorCode(),
        response.isSuccess(), null, createResponse);
  }

  /**
   * Set Product Type based on Merchant originId
   * @param bpProfile
   * @param request
   */
  private void setProductTypeByMerchantOriginId(ProfileResponse bpProfile,
      ProductBusinessPartnerServiceRequest request) {
    if (bpProfile.getCompany().isInternationalFlag()
        && isDefaultMerchantInternationalOriginIdPresent(request)){
      for (ProductItemBusinessPartnerServiceRequest productItemBusinessPartnerRequest : request
          .getProductItemBusinessPartners()) {
        productItemBusinessPartnerRequest.setProductType(ProductType.BIG_PRODUCT.getCode());
      }
    }
  }

  @Override
  public BusinessPartnerProfileWebResponse getBusinessPartnerProfile(String businessPartnerCode) {
    if (StringUtils.isBlank(businessPartnerCode)) {
      throw new ValidationException(
          ErrorCategory.REQUIRED_PARAMETER.getMessage() + ErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_EMPTY);
    }
    ProfileResponse response =
      businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    List<PickupPointOutboundResponse> pickupPointCodeResponseList =
      businessPartnerService.getAllPickupPointsForBusinessPartner(businessPartnerCode);
    ResponseHelper.addPickupPointDetailsInProfileResponse(response, pickupPointCodeResponseList);
    return ResponseHelper.toBusinessPartnerProfileWebResponse(response, bpBopisRestrictionEnabled,businessPartnerCode, distributionSellerList, ranchIntegrationEnabled);
  }

  /**
   * Is DefaultMerchantInternationalOriginId present in merchant's pickuppoint
   * @param request
   * @return
   */
  private boolean isDefaultMerchantInternationalOriginIdPresent(ProductBusinessPartnerServiceRequest request) {
    PickupPointOutboundResponse pickupPointResponse =
        businessPartnerService.getPickupPointByCode(request.getProductItemBusinessPartners().get(0).getPickupPointId());
    return systemParameterProperties.getDefaultMerchantInternationalOriginId()
        .equals(pickupPointResponse.getOriginId());
  }

  @Override
  public GdnBaseRestResponse copy(ProductCopyRequest productCopyRequest, boolean isRetryAttempt) {
    PickupPointWebResponse pickupPoint = getDefaultPickupPoint(productCopyRequest.getBusinessPartnerCode());
    productCopyRequest.setPickupPointCode(pickupPoint.getCode());
    GdnBaseRestResponse response = pbpFeign.copy(isRetryAttempt, productCopyRequest);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse copyAll(ProductCopyRequest productCopyRequest) {
    PickupPointWebResponse pickupPoint = getDefaultPickupPoint(productCopyRequest.getBusinessPartnerCode());
    productCopyRequest.setPickupPointCode(pickupPoint.getCode());
    GdnBaseRestResponse response = pbpFeign.copyAll(productCopyRequest);
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public GdnRestListResponse<AvailableToCopyProductDetailsResponse> productsAvailableToDirectCopy(String businessPartnerCode,
    String linkedPartnerCode, int page, int size, ProductLevel3SummaryRequest summaryRequest) {
    log.debug("products available to be copied from {} to {}", linkedPartnerCode, businessPartnerCode);
    GdnRestListResponse<AvailableToCopyProductDetailsResponse> response = pbpFeign
      .productsAvailableToCopy(businessPartnerCode, linkedPartnerCode, page, size, summaryRequest);

    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public boolean isProductMappedToMerchant(String merchantCode) {
    GdnRestSimpleResponse<Boolean> productMappedToMerchant = pbpFeign.isProductMappedToMerchant(merchantCode);
    ResponseHelper.validateResponse(productMappedToMerchant);
    return productMappedToMerchant.getValue();
  }

  private PickupPointWebResponse getDefaultPickupPoint(String businessPartnerCode) {
    try {
      PickupPointFilterRequest pickupPointFilterRequest =
          PickupPointFilterRequest.builder().businessPartnerCode(businessPartnerCode).defaultAddress(true).build();
      RequestHelper.setWaitingDeletionFlagForDeletePickupPoint(setWaitingDeletionForDeletePickupPoint,
          pickupPointFilterRequest);
      Page<PickupPointOutboundResponse>
          pickupPointResponseList = businessPartnerService.filterBusinessPartner(0, 1, pickupPointFilterRequest);
      if (CollectionUtils.isNotEmpty(pickupPointResponseList.getContent())) {
        PickupPointOutboundResponse pickupPointResponse = pickupPointResponseList.getContent().get(0);
        return ResponseHelper.toPickupPointWebResponse(pickupPointResponse);
      }
      else {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "no pickup point found");
      }
    } catch(Exception e) {
      log.warn("failed to get business partner details : {} ", businessPartnerCode, e);
    }
    throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "failed to get pickup point details");
  }

  private PickupPointWebResponse getAnyPickupPoint(List<PickupPointWebResponse> pickupPoints) {
    return pickupPoints.stream()
      .findFirst()
      .orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "no pickup point found"));
  }

  @Override
  public void updateDefaultConfigurationsAndPickupPoints(String storeId, String merchantCode, DefaultConfigurationAndPickupPointRequest request) {
    if (request.isPickupCodesUpdated()) {
      businessPartnerService.updateDefaultPickupPointCode(RequestHelper.toMarkPickupPointAsDefaultRequest(merchantCode, request));
    }
    if (request.isDefaultConfigurationUpdated()) {
      businessPartnerService.updateDefaultConfiguration(RequestHelper.toProfileRequest(merchantCode, request));
    }
  }

}
