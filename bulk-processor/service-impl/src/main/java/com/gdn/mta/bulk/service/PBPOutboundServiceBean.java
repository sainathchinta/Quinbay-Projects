package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.gda.mta.product.dto.BrandUpdateRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.ItemErrorListResponse;
import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.DeleteInProgressL5Response;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gda.mta.product.dto.response.ProductCodeAndNameResponseList;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.models.ProductBrandUpdateRequest;
import com.gdn.mta.bulk.models.ProductMasterDataEditRequest;
import com.gdn.mta.bulk.dto.product.ProductAndBrandResponse;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

@Service
@Slf4j
public class PBPOutboundServiceBean implements PBPOutboundService {

  public static final String SYSTEM_ERROR = "System error";
  @Autowired
  private PBPFeign pbpFeign;

  @Value("${fetch.batch.size.for.nr.product.details}")
  private int fetchBatchSizeForNRProductDetails;

  @Value("${bulk.brand.update.products.fetch.size}")
  private int bulkBrandUpdateProductsFetchSize;

  @Override
  public List<PredefinedAllowedAttributeValueResponse> getActiveBrandsByCategoryId(String categoryId) throws Exception {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response = this.pbpFeign.getActiveBrandsByCategoryId(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), categoryId, true);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public Page<InProgressProductResponse> fetchInProgressProductsByMerchantCode(String requestId, String username,
      String businessPartnerCode, Pageable pageable) {
    GdnRestListResponse<InProgressProductResponse> response =
        this.pbpFeign.fetchInProgressProductsByMerchantCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            requestId, username, businessPartnerCode, pageable.getPageNumber(), pageable.getPageSize());
    if (!response.isSuccess()) {
      log.error("Error fetching in-progress products for seller : {}, error {}", businessPartnerCode,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), pageable, response.getContent().size());
  }

  @Override
  public void updateProductItemViewConfig(String productSku,
      ProductLevel3ViewConfigStockRequest productLevel3ViewConfigStockRequest) throws Exception {
    GdnBaseRestResponse response =
        this.pbpFeign.updateProductItemViewConfig(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, productSku, productLevel3ViewConfigStockRequest);
    if (!response.isSuccess()) {
      log.error("Error updating product productSku : {} viewConfig in-progress products for seller, error {}", productSku,
          response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public Page<ItemPickupPointListingL3Response> findProductDetailsFromL5(String businessPartnerCode,
      ProductLevel3SummaryRequest request, Pageable pageable, boolean needCorrection) throws Exception {
    GdnRestListResponse<ItemPickupPointListingL3Response> response =
        this.pbpFeign.getItemPickupPointL3Listing(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, pageable.getPageNumber(), pageable.getPageSize(),
            ConverterUtil.toItemPickupPointListingL3Request(request, businessPartnerCode, needCorrection));
    if (!response.isSuccess() || response.getContent().isEmpty()) {
      log.error("While getting Product Details From L5 got PBP client error, businessPartnerCode : {} , error : ",
          businessPartnerCode, response.getErrorMessage());
      throw new Exception("PBP client error: " + response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), pageable, response.getPageMetaData().getTotalRecords());
  }

  @Override
  public boolean listingUpdate(ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request, String productSku)
      throws Exception {
    GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> response =
        this.pbpFeign.listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, productSku, productLevel3QuickEditV2Request);
    if (Optional.ofNullable(response.getValue())
        .map(ItemPriceStockQuickUpdateResponse::getVariantsErrorList)
        .filter(CollectionUtils::isNotEmpty).isPresent()) {
      for (ItemErrorListResponse itemErrorListResponse : response.getValue().getVariantsErrorList()) {
        log.error(
            "Error while updating L5 using listingUpdate, productSku : {} , itemPickupPointId : {} , errorMessage - ",
            productSku, itemErrorListResponse.getItemPickupPointId(), itemErrorListResponse.getErrorMessage());
      }
      if (!response.isSuccess() || Objects.nonNull(response.getValue().getApiErrorCode())) {
        log.error("Error while updating L5 using listingUpdate, productSku : {}, ApiErrorCode : {} , error - ",
            productSku, response.getValue().getApiErrorCode(), response.getErrorMessage());
        throw new Exception(
            "Error while updating using listingUpdate, ApiErrorCode : " + response.getValue().getApiErrorCode()
                + ", error - " + response.getErrorMessage() + " VariantErrorList - " + response.getValue()
                .getVariantsErrorList());
      }
      throw new Exception(
          "Error while updating using listingUpdate, VariantErrorList - " + response.getValue().getVariantsErrorList());
    } else if (!response.isSuccess()) {
      log.error("Error while updating L5 using listingUpdate, productSku : {}, ApiErrorCode : {} , "
              + "error - ", productSku, Optional.ofNullable(response.getValue())
              .map(ItemPriceStockQuickUpdateResponse::getApiErrorCode).orElse(null),
          response.getErrorMessage());
      throw new Exception(
          "Error while updating using listingUpdate, ApiErrorCode : " + Optional.ofNullable(
                  response.getValue()).map(ItemPriceStockQuickUpdateResponse::getApiErrorCode)
              .orElse(null) + ", error - " + response.getErrorMessage());
    }
    return response.isSuccess();
  }

  public Page<ProductLevel3SummaryResponse> fetchProductLevel3SummaryByProductSkuList(int page,
    int size, String businessPartnerCode, boolean onlineOrCnc, List<String> productSkuList)
    throws ApplicationException {
    GdnRestListResponse<ProductLevel3SummaryResponse> response =
      this.pbpFeign.getL5SummaryByProductSkuList(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, page, size, businessPartnerCode, onlineOrCnc,
        new SimpleListStringRequest(productSkuList));
    if (!response.isSuccess()) {
      log.error("Error fetching L5 details by productSkuList : {}, page : {}, size : {}, "
          + "merchantCode : {}, onlineOrCnc : {}  error {}", productSkuList, page, size,
        businessPartnerCode, onlineOrCnc, response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
      response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void createNewProduct(String requestId, String createdBy,
    ProductCreationRequest productCreationRequest) throws Exception {
    log.info("request : {} ", productCreationRequest);
    GdnBaseRestResponse response =
        this.pbpFeign.createNewProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, requestId, createdBy,
            productCreationRequest);
    if (!response.isSuccess()) {
      log.error("Error while creating the product with product sku = {} , {} ",
          productCreationRequest.getGdnProductSku(), response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public Page<ItemPickupPointListingL3Response> getItemPickupPointListingL3Response(int page, int size,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) throws Exception {
    GdnRestListResponse<ItemPickupPointListingL3Response> response =
        this.pbpFeign.getItemPickupPointListingL3Response(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, page, size, false, itemPickupPointListingL3Request);
    if (!response.isSuccess()) {
      log.error("Error while getting itemPickupPoint l3 listing response with  product sku = {} , {} ",
          itemPickupPointListingL3Request.getProductSku(), response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse updateSummaryL5(String createdBy,
      ProductVariantUpdateRequest productVariantUpdateRequest, String clientHost) throws Exception {
    GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> response =
        this.pbpFeign.updateSummaryL5(Constant.STORE_ID, Constant.CHANNEL_ID, clientHost, Constant.REQUEST_ID,
            createdBy, productVariantUpdateRequest);
    if (!response.isSuccess()) {
      log.error("Error while updating itemPickupPoint  with  product sku = {} , {} ",
          productVariantUpdateRequest.getProductSku(), response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void deleteTerminatedSellerProducts(String productSku) {
    GdnBaseRestResponse response =
        this.pbpFeign.deleteTerminatedSellerProducts(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, productSku);
    if (!response.isSuccess()) {
      log.error("While deleteing terminated seller products. productSku : {} ", productSku);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public List<DeleteInProgressL5Response> deleteL5ByPickupPointCode(
      ItemSkuPickupPointRequest itemSkuPickupPointRequest) {
    GdnRestListResponse<DeleteInProgressL5Response> response =
        this.pbpFeign.deleteL5ByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, itemSkuPickupPointRequest);
    if (!response.isSuccess()) {
      log.error("Error while deleting L5 with ItemSkuPickupPointRequest = {} ,Error = {} ", itemSkuPickupPointRequest,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public Page<InProgressProductsByPickupPointCodeResponse> getInProgressProductsByPickupPointCode(
      String businessPartnerCode, String pickupPointCode, int page, int size) {
    GdnRestListResponse<InProgressProductsByPickupPointCodeResponse> response =
        this.pbpFeign.getInProgressProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, businessPartnerCode, pickupPointCode, page, size);
    if (!response.isSuccess()) {
      log.error("Error while fetching in progress L5's with businessPartnerCode = {} and pickupPointCode = {}  ",
          businessPartnerCode, pickupPointCode, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public FbbCreatePickupPointResponse createDefaultL5Fbb(
    FbbCreatePickupPointRequest createPickupPointRequest) throws Exception {
    GdnRestSingleResponse<FbbCreatePickupPointResponse> response =
      this.pbpFeign.createDefaultL5Fbb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID
        , Constant.REQUEST_ID, Constant.USER_NAME,
        createPickupPointRequest);
    if (!response.isSuccess()) {
      log.error("Error while creating default fbb l5 for itemSku {} & error {} ",
        createPickupPointRequest.getItemSku(), response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  public void updateBrandOfProduct(String storeId, ProductBrandUpdateRequest productBrandUpdateRequest) {
    GdnBaseRestResponse response = this.pbpFeign.updateBrandOfProduct(storeId, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, productBrandUpdateRequest.getProductCode(),
        new BrandUpdateRequest(productBrandUpdateRequest.getOldBrandCode(),
            productBrandUpdateRequest.getNewBrandCode()));
    if (!response.isSuccess()) {
      log.error("Exception while updating product brand productCode {} & error {} ",
          productBrandUpdateRequest.getProductCode(), response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public GdnBaseRestResponse updateProductBrandName(
      InternalBrandUpdateEventModel internalBrandUpdateEventModel,
      BulkInternalProcessData bulkInternalProcessData) {
    com.gdn.mta.bulk.dto.ProductBrandUpdateRequest productBrandUpdateRequest =
        getProductBrandUpdateRequest(internalBrandUpdateEventModel, bulkInternalProcessData);
    GdnBaseRestResponse response =
        this.pbpFeign.updateProductBrandName(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productBrandUpdateRequest);

    if (Objects.isNull(response) || Optional.ofNullable(response.getErrorCode())
        .map(code -> ErrorCategory.UNSPECIFIED.getCode().contains(code))
        .orElse(false)) {
      log.error("Exception while updating product brand name for productCode {}  ",
          productBrandUpdateRequest.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "Failed to update the brand due to a PBP service error");
    }
    return response;
  }

  private static com.gdn.mta.bulk.dto.ProductBrandUpdateRequest getProductBrandUpdateRequest(
      InternalBrandUpdateEventModel internalBrandUpdateEventModel,
      BulkInternalProcessData bulkInternalProcessData) {
    com.gdn.mta.bulk.dto.ProductBrandUpdateRequest productBrandUpdateRequest =
        new com.gdn.mta.bulk.dto.ProductBrandUpdateRequest();
    productBrandUpdateRequest.setNewBrandName(internalBrandUpdateEventModel.getNewBrandName());
    productBrandUpdateRequest.setOldBrandName(internalBrandUpdateEventModel.getOldBrandName());
    productBrandUpdateRequest.setOnlyBrandNameUpdate(
        BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name()
            .equals(bulkInternalProcessData.getProcessType()));
    productBrandUpdateRequest.setNewBrandCode(internalBrandUpdateEventModel.getNewBrandCode());
    productBrandUpdateRequest.setOldBrandCode(internalBrandUpdateEventModel.getOldBrandCode());
    productBrandUpdateRequest.setProductCode(bulkInternalProcessData.getParentCode());
    productBrandUpdateRequest.setBrandLevelUpdateRequired(
        internalBrandUpdateEventModel.isBrandNameUpdate());
    return productBrandUpdateRequest;
  }

  @Override
  public ProductCodeAndNameResponseList getProductDetailsOfBusinessPartnerFromPBP(String storeId, String requestId,
      String businessPartnerCode) {
    ProductCodeAndNameResponseList productCodeAndNameResponseList = new ProductCodeAndNameResponseList();
    List<ProductCodeAndNameDetails> productCodeAndNameDetailsList = new ArrayList<>();
    int currentPage = 0;
    GdnRestListResponse<ProductCodeAndNameDetails> productCodeAndNameResponse;
    do {
      productCodeAndNameResponse =
          this.pbpFeign.getProductsByBusinessPartnerCodeFromLastXDays(storeId, requestId, businessPartnerCode,
              currentPage, fetchBatchSizeForNRProductDetails);
      if (!productCodeAndNameResponse.isSuccess() || CollectionUtils.isEmpty(productCodeAndNameResponse.getContent())) {
        log.error("Error while fetching products for businessPartner {} and requestId {} ", businessPartnerCode,
            requestId);
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, productCodeAndNameResponse.getErrorMessage());
      }
      productCodeAndNameDetailsList.addAll(productCodeAndNameResponse.getContent());
      currentPage++;
    } while (currentPage < Math.ceil(
        (double) productCodeAndNameResponse.getPageMetaData().getTotalRecords() / fetchBatchSizeForNRProductDetails));
    productCodeAndNameResponseList.setProducts(productCodeAndNameDetailsList);
    return productCodeAndNameResponseList;
  }


  @Override
  public void updatedProductMaterData(String productSku,
      ProductMasterDataEditRequest productMasterDataEditRequest, String username) {
    log.info("Updating product master data for productSku: {} with request: {}", productSku,
        productMasterDataEditRequest);
    GdnBaseRestResponse editResponse =
        pbpFeign.updatedProductMaterData(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, username, productSku, productMasterDataEditRequest);
    if (editResponse.isSuccess()) {
      if (StringUtils.isNotBlank(editResponse.getErrorMessage())) {
        log.error(
            "Validation error while updating product master data for productSku: {}, error: {}",
            productSku, editResponse.getErrorMessage());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            editResponse.getErrorMessage());
      }
    } else {
      log.error("System error while updating product master data for productSku: {}, error: {}",
          productSku, editResponse.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, SYSTEM_ERROR);
    }
  }

  @Override
  public GdnRestListResponse<ProductAndBrandResponse> getProductAndBrandResponseGdnRestListResponse(
      int page, String destinationBrandName, String storeId) {
    String channelId = GdnMandatoryRequestParameterUtil.getChannelId();
    String clientId = GdnMandatoryRequestParameterUtil.getClientId();
    String requestId = GdnMandatoryRequestParameterUtil.getRequestId();
    String username = GdnMandatoryRequestParameterUtil.getUsername();

    GdnRestListResponse<ProductAndBrandResponse> response =
        pbpFeign.getProductsByBrandName(storeId, channelId, clientId, requestId, username,
            destinationBrandName, page, bulkBrandUpdateProductsFetchSize);

    if (!response.isSuccess()) {
      log.error("Exception while getting response from PBP for brand : {} ", destinationBrandName);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    if (CollectionUtils.isEmpty(response.getContent())) {
      log.error("No products for brand : {} ", destinationBrandName);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "no product found for brand");
    }
    return response;
  }

}
