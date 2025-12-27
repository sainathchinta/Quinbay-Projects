package com.gdn.mta.bulk.repository;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gda.mta.product.dto.BusinessPartnerCodeResponseList;
import com.gda.mta.product.dto.CreateProductRequest;
import com.gda.mta.product.dto.NeedRevisionProductsRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.StoreCopyConstants;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.extern.slf4j.Slf4j;

@Repository
@Slf4j
public class ProductRepositoryBean implements ProductRepository {

  public static final String NEED_CORRECTION = "NEED_CORRECTION";

  @Autowired
  private PBPFeign pbpFeign;

  @Value("${fetch.nr.bp.codes.from.last.x.days}")
  private int fetchNRBPCodesFromLastXDays;

  @Value("${fetch.batch.size.for.nr.bp.codes}")
  private int fetchBatchSizeForNRBPCodes;
  
  @Override
  public ProductDetailResponse findProductDetailByProductCodeAndMarkForDeleteFalse(String productCode)
      throws Exception {
    GdnRestSingleResponse<ProductDetailResponse> response =
        pbpFeign.filterProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
             false, productCode);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()), response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void update(ProductRequest product) throws Exception {
    GdnBaseRestResponse response = this.pbpFeign.updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), product);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()), response.getErrorMessage());
    }
  }

  @Override
  public String generateBarcode(String requestId) throws Exception {
    GdnRestSimpleResponse<String> response = this.pbpFeign.generateBarcode(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername());
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()),
          response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public String create(CreateProductRequest request) throws Exception {
    GdnRestSimpleResponse<String> response = this.pbpFeign.create(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()), response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public String createViaApi(CreateProductRequest request) throws Exception {
    GdnRestSimpleResponse<String> response = this.pbpFeign.create(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
        request);
    if (!response.isSuccess()) {
      throw new ApplicationException(
          ErrorCategory.valueOf(response.getErrorCode()), response.getErrorMessage());
    }

    return response.getValue();
  }

  @Override
  public BulkMasterProductUpdateResponse updateMasterProducts(BulkMasterProductUpdateRequest request)
    throws Exception {
    GdnRestSingleResponse<BulkMasterProductUpdateResponse> response =
        pbpFeign.updateActivatedProductsBulkMasterData(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()),
          response.getErrorMessage());
    }
    return response.getValue();
  }

  public GdnRestSimpleResponse<String> generateProductCode(String requestId, String username)
      throws Exception {
    GdnRestSimpleResponse<String> response =
        this.pbpFeign.generateProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
            username);
    validateClientResponse(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse createProduct(String requestId, String username,
      ProductCreationRequest request) throws Exception {
    GdnBaseRestResponse response = this.pbpFeign
        .createNewProduct(request.getStoreId(), StoreCopyConstants.CHANNEL_ID, StoreCopyConstants.CLIENT_ID, requestId,
            username, request);
    validateClientResponse(response);
    return response;
  }
  
  private void validateClientResponse(GdnBaseRestResponse response) throws ApplicationException {
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.VALIDATION, response.getErrorMessage());
    }
  }

  public List<String> fetchNRBusinessPartnerCodes(String storeId, String requestId) {
    NeedRevisionProductsRequest needRevisionProductsRequest = getNeedRevisionProductsRequest();
    List<String> allBusinessPartnerCodes = new ArrayList<>();
    int currentPage = 0;
    GdnRestListResponse<BusinessPartnerCodeResponseList> businessPartnerCodesResponse;
    do {
      businessPartnerCodesResponse =
          this.pbpFeign.getDistinctBusinessPartnerCodeForRevisedProducts(storeId, requestId, currentPage,
              fetchBatchSizeForNRBPCodes, needRevisionProductsRequest);
      if (!businessPartnerCodesResponse.isSuccess() || CollectionUtils.isEmpty(
          businessPartnerCodesResponse.getContent())) {
        log.error("Error while fetching businessPartnerCodes for requestId: {} at page: {} : {}", requestId,
            currentPage, businessPartnerCodesResponse.getErrorMessage());
        throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
            businessPartnerCodesResponse.getErrorMessage());
      }
      List<String> businessPartCodeList = Optional.ofNullable(businessPartnerCodesResponse.getContent().get(0))
          .orElse(new BusinessPartnerCodeResponseList()).getBusinessPartCodeList();
      if (CollectionUtils.isNotEmpty(businessPartCodeList)) {
        allBusinessPartnerCodes.addAll(businessPartCodeList);
      }
      currentPage++;
    } while (currentPage < Math.ceil(
        (double) businessPartnerCodesResponse.getPageMetaData().getTotalRecords() / fetchBatchSizeForNRBPCodes));
    return allBusinessPartnerCodes;
  }


  private NeedRevisionProductsRequest getNeedRevisionProductsRequest() {
    NeedRevisionProductsRequest needRevisionProductsRequest = new NeedRevisionProductsRequest();
    needRevisionProductsRequest.setState(NEED_CORRECTION);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_YEAR, -fetchNRBPCodesFromLastXDays);
    Date startDate = calendar.getTime();
    needRevisionProductsRequest.setStartUpdatedDate(startDate);
    needRevisionProductsRequest.setEndUpdatedDate(new Date());
    return needRevisionProductsRequest;
  }

  @Override
  public List<NeedRevisionEligibilityResponse> getEligibilityForNeedRevisionDeletion(String storeId,
    List<NeedRevisionEligibilityRequest> needRevisionEligibilityRequest)
    throws ApplicationException {
    GdnRestListResponse<NeedRevisionEligibilityResponse> needRevisionEligibilityResponse =
      pbpFeign.eligibilityForNeedRevisionDeletion(storeId,
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), needRevisionEligibilityRequest);
    if (!needRevisionEligibilityResponse.isSuccess() || Objects.isNull(
      needRevisionEligibilityResponse.getContent())) {
      log.error(
        "Error while Getting Eligibility for Need revision deletion of seller : {}, " + "error : "
          + "{}", needRevisionEligibilityRequest.stream()
          .map(NeedRevisionEligibilityRequest::getBusinessPartnerCode),
        needRevisionEligibilityResponse.getErrorMessage());
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
        needRevisionEligibilityResponse.getErrorMessage());
    } return needRevisionEligibilityResponse.getContent();
  }

  @Override
  public boolean deleteProductCollection(String storeId,
    DeleteProductRequest deleteProductRequest , boolean needEmailNotification) {
    GdnBaseRestResponse response =
      pbpFeign.deleteProductCollection(storeId, GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), needEmailNotification,
        deleteProductRequest);
    return response.isSuccess();
  }

}
