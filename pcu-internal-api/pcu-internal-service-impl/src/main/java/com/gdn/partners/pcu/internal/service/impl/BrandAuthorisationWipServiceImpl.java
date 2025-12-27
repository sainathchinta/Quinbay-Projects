package com.gdn.partners.pcu.internal.service.impl;
import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.partners.pcu.internal.model.Constants.NEAR_EXPIRY;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.EMPTY_DOCUMENT_ERROR;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.EMPTY_IPR_REGISTRATION_NUMBER_EXCEEDED_MAX_LENGTH;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthUpdateRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipActionRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipListRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthWipDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthCreateWipResponse;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthorisationWipListResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.BPService;
import com.gdn.partners.pcu.internal.service.BrandAuthorisationWipService;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthorisationWipActionWebRequest;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class BrandAuthorisationWipServiceImpl implements BrandAuthorisationWipService {

  private final PCBFeign pcbFeign;
  private final FileStorageService fileStorageService;
  private final BPService bpService;

  @Value("${brand.auth.end.date.years.add:5}")
  private int numberOfYears;

  @Value("${brand.auth.wip.near.expiry.days.threshold}")
  private Integer brandAuthWipNearExpiryDaysThreshold;

  @Override
  public BrandAuthWipDetailResponse fetchBrandAuthWipDetails(String storeId, String status, String id,
      String sellerCode) {
    RequestHelper.checkParameter(StringUtils.isNotEmpty(storeId),
      ErrorMessages.EMPTY_STORE_ID_ERROR);
    log.info("Fetch brand auth wip details with id {}", id);
    GdnRestSingleResponse<BrandAuthWipDetailResponse> brandAuthorisationDetail =
      pcbFeign.fetchBrandAuthWipDetails(storeId, status, id);
    ResponseHelper.validateMasterSkuResponse(brandAuthorisationDetail);
    ProfileResponse profileResponse = bpService.getProfileResponseByBusinessPartnerCode(
      brandAuthorisationDetail.getValue().getSellerCode());
    brandAuthorisationDetail.getValue().setSellerName(
        Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany)
            .map(CompanyDTO::getBusinessPartnerName).orElse(StringUtils.EMPTY));
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(brandAuthorisationDetail.getValue().getAuthExpireDate());
    calendar.add(Calendar.DAY_OF_MONTH, -brandAuthWipNearExpiryDaysThreshold);
    Date thresholdDate = calendar.getTime();
    if (BrandAuthorisationStatus.ACTIVE.name()
        .equalsIgnoreCase(brandAuthorisationDetail.getValue().getStatus())) {
      if (brandAuthorisationDetail.getValue().getAuthExpireDate().before(date)) {
        brandAuthorisationDetail.getValue().setStatus(BrandAuthorisationStatus.EXPIRED.name());
      } else if (brandAuthorisationDetail.getValue().getAuthStartDate().after(date)) {
        brandAuthorisationDetail.getValue().setStatus(BrandAuthorisationStatus.INACTIVE.name());
      } else if (thresholdDate.before(date)){
        brandAuthorisationDetail.getValue().setStatus(NEAR_EXPIRY);
      }
    }
    if (StringUtils.isBlank(sellerCode) || sellerCode.equals(
        brandAuthorisationDetail.getValue().getSellerCode())) {
      return brandAuthorisationDetail.getValue();
    }
    else{
      return null;
    }
  }

  @Override
  public void brandAuthorisationWipAction(String storeId, String username,
      BrandAuthorisationWipActionWebRequest brandAuthorisationWipActionWebRequest) {
    BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest =
        new BrandAuthorisationWipActionRequest();
    BeanUtils.copyProperties(brandAuthorisationWipActionWebRequest,
        brandAuthorisationWipActionRequest);
    GdnBaseRestResponse response =
        pcbFeign.brandAuthorisationWipAction(storeId, username, brandAuthorisationWipActionRequest);
    ResponseHelper.validateMasterSkuResponse(response);
  }


  @Override
  public boolean validateBrandAuthRequest(String storeId, String brandCode, String sellerCode,
    boolean edited) {
    RequestHelper.checkParameter(StringUtils.isNotEmpty(storeId),
        ErrorMessages.EMPTY_STORE_ID_ERROR);
    RequestHelper.checkParameter(StringUtils.isNotEmpty(brandCode),
        ErrorMessages.EMPTY_BRAND_CODE_ERROR);
    RequestHelper.checkParameter(StringUtils.isNotEmpty(sellerCode),
        ErrorMessages.EMPTY_SELLER_CODE_ERROR);
    log.info("Validating brand auth request for brandCode: {} , sellerCode: {}", brandCode,
        sellerCode);
    GdnRestSingleResponse<SimpleBooleanResponse> response =
      pcbFeign.validateBrandAuthRequest(storeId, edited, brandCode, sellerCode);
    ResponseHelper.validateMasterSkuResponse(response);
    return response.getValue().getResult();
  }

  @Override
  public BrandAuthCreateWipResponse createBrandAuthRequest(String storeId, String username,
      String requestId, BrandAuthCreateWipRequest brandAuthCreateWipRequest) {
    BrandAuthCreateWipResponse response = new BrandAuthCreateWipResponse();
    RequestHelper.validateBrandAuthWipRequest(brandAuthCreateWipRequest, numberOfYears);
    BrandAuthCreateWipRequest brandAuthRequest =
        ConverterUtil.toCreateBrandWipRequest(brandAuthCreateWipRequest);
    validateDocumentLinks(brandAuthRequest.getDocumentLinks(), brandAuthRequest.getSellerCode(),
        brandAuthRequest.getBrandCode());
    GdnRestSingleResponse<BrandAuthCreateWipResponse> brandAuthCreateResponse =
        pcbFeign.createBrandAuthWip(storeId, requestId, username, brandAuthRequest);
    ResponseHelper.validateResponse(brandAuthCreateResponse);
    log.info("New Brand Auth created for brand code : {} ",
        brandAuthCreateResponse.getValue().getBrandCode());
    BeanUtils.copyProperties(brandAuthCreateResponse.getValue(), response);
    return response;
  }

  @Override
  public void updateBrandAuthWip(String storeId, String username,
      BrandAuthUpdateRequest brandAuthUpdateRequest) {
    RequestHelper.checkParameter(StringUtils.isNotEmpty(storeId),
        ErrorMessages.EMPTY_STORE_ID_ERROR);
    RequestHelper.checkParameter(StringUtils.isNotEmpty(brandAuthUpdateRequest.getBrandCode()),
        ErrorMessages.EMPTY_BRAND_CODE_ERROR);
    RequestHelper.checkParameter(StringUtils.isNotEmpty(brandAuthUpdateRequest.getSellerCode()),
        ErrorMessages.EMPTY_SELLER_CODE_ERROR);
    GdnPreconditions.checkArgument(brandAuthUpdateRequest.getIprRegistrationNumber().length()
            <= Constants.MAX_LENGTH_IPR_REGISTRATION_NUMBER,
        EMPTY_IPR_REGISTRATION_NUMBER_EXCEEDED_MAX_LENGTH);
    GdnPreconditions.checkArgument(
        CollectionUtils.isNotEmpty(brandAuthUpdateRequest.getDocumentLinks()),
        EMPTY_DOCUMENT_ERROR);
    GdnBaseRestResponse response =
        pcbFeign.brandAuthorisationWipUpdate(storeId, username, brandAuthUpdateRequest);
    ResponseHelper.validateErrorCodeResponse(response);
  }

  @Override
  public Page<BrandAuthorisationWipListResponse> getBrandAuthorisationWipList(int page, int size,
      BrandAuthorisationWipListRequest brandAuthorisationWipListRequest) {
    GdnRestListResponse<BrandAuthorisationWipListResponse> response =
        pcbFeign.filterSummary(page, size, brandAuthorisationWipListRequest);
    ResponseHelper.validateMasterSkuResponse(response);
    List<BrandAuthorisationWipListResponse> brandAuthorisationWipListResponses =
        ResponseHelper.toBrandAuthorisationWipResponseList(response.getContent(), brandAuthWipNearExpiryDaysThreshold);
    return new PageImpl<>(brandAuthorisationWipListResponses, PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  private void validateDocumentLinks(List<String> documents, String sellerCode, String brandCode) {
    if (CollectionUtils.isNotEmpty(documents)) {
      checkArgument(documents.stream().noneMatch(StringUtils::isEmpty),
          ErrorMessage.FILE_NAME_SHOULD_NOT_BE_NULL_OR_EMPTY.getMessage());
      validateAuthDocument(documents, brandCode, sellerCode);
    }
  }

  private void validateAuthDocument(List<String> documentList, String brandCode,
      String sellerCode) {
    for (String fileName : documentList) {
      fileStorageService.checkIfFileExisting(fileName, brandCode, sellerCode);
    }
  }

  @Override
  public boolean checkEligibility(String sellerCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response = pcbFeign.creationEligibility(sellerCode);
    ResponseHelper.validateMasterSkuResponse(response);
    return Optional.ofNullable(response.getValue())
        .orElseGet(() -> new SimpleBooleanResponse(false)).getResult();
  }
}
