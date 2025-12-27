package com.gdn.partners.pcu.internal.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.Objects;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.product.DownloadUnmappedSkuDomainEventModel;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.feign.XProductFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.ProductCenterService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.enums.ProductCenterActionEnum;
import com.gdn.partners.pcu.internal.web.model.request.CurrentCategoryCatalogWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterListingActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterSummaryWebResponse;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductCenterServiceImpl implements ProductCenterService {

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public Page<ProductCenterSummaryWebResponse> getProductCenterFilterSummary(String storeId, String requestId,
      ProductCenterSummaryWebRequest productCenterSummaryWebRequest, Integer page, Integer size) throws Exception {
    if (Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY)) {
      GdnRestListResponse<ProductCenterSummaryResponse> response = xProductFeign
          .getProductCenterSummaryFilter(storeId, requestId, page, size,
              RequestHelper.toProductCenterSummaryRequest(productCenterSummaryWebRequest));
      ResponseHelper.validateResponse(response);
      return new PageImpl<>(ResponseHelper.toListProductCenterSummaryWebResponse(response.getContent()),
          PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  @Override
  public void updateProductCenterListing(String storeId, String requestId,
      ProductCenterListingActionWebRequest productCenterListingActionWebRequest) throws Exception {
    List<String> productSkus = productCenterListingActionWebRequest.getProductSKus();
    if (Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY)) {
      if (ProductCenterActionEnum.ADD.name().equals(productCenterListingActionWebRequest.getAction())) {
        addSalesCategory(storeId, requestId, productCenterListingActionWebRequest.getNewCatalogCode(),
            productCenterListingActionWebRequest.getNewCategoryCode(), productSkus);
      } else if (ProductCenterActionEnum.DELETE.name().equals(productCenterListingActionWebRequest.getAction())) {
        for (CurrentCategoryCatalogWebRequest currentCategoryCatalogWebRequest : productCenterListingActionWebRequest
            .getCurrentCategoryCodeSelected()) {
          deleteSalesCategory(storeId, requestId, currentCategoryCatalogWebRequest.getCatalogCode(),
              currentCategoryCatalogWebRequest.getCategoryCode(), productSkus);
        }
      } else if (ProductCenterActionEnum.MOVE.name().equals(productCenterListingActionWebRequest.getAction())) {
        moveSalesCategory(storeId, requestId,
            productCenterListingActionWebRequest.getCurrentCategoryCodeSelected().get(0).getCatalogCode(),
            productCenterListingActionWebRequest.getCurrentCategoryCodeSelected().get(0).getCategoryCode(),
            productCenterListingActionWebRequest.getNewCategoryCode(), productSkus);
        if (productCenterListingActionWebRequest.getCurrentCategoryCodeSelected().size() > 1) {
          for (CurrentCategoryCatalogWebRequest currentCategoryCatalogWebRequest : productCenterListingActionWebRequest
              .getCurrentCategoryCodeSelected()
              .subList(1, productCenterListingActionWebRequest.getCurrentCategoryCodeSelected().size())) {
            deleteSalesCategory(storeId, requestId, currentCategoryCatalogWebRequest.getCatalogCode(),
                currentCategoryCatalogWebRequest.getCategoryCode(), productSkus);
          }
        }
      }
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  @Override
  public void downloadUnmappedSkus(String storeId, String requestId, String username, String parentCategoryCode,
      String language) throws Exception {
    if (Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY)) {
      kafkaPublisher.send(DomainEventName.DOWNLOAD_UNMAPPED_SKUS, username,
          new DownloadUnmappedSkuDomainEventModel(storeId, username, username, requestId, parentCategoryCode,
              language));
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  private void addSalesCategory(String storeId, String requestId, String newCatalogCode, String newCategoryCode,
      List<String> productSkus) throws Exception {
    GdnBaseRestResponse response = xProductFeign
        .addProductSalesCatalog(storeId, requestId, newCatalogCode, newCategoryCode,
            new SimpleListStringRequest(productSkus));
    ResponseHelper.validateResponse(response);
  }

  private void deleteSalesCategory(String storeId, String requestId, String oldCatalogCode, String oldCategoryCode,
      List<String> productSkus) throws Exception {
    GdnBaseRestResponse response = xProductFeign.deleteSalesCatalog(storeId, requestId, oldCatalogCode, oldCategoryCode,
        new SimpleListStringRequest(productSkus));
    ResponseHelper.validateResponse(response);
  }

  private void moveSalesCategory(String storeId, String requestId, String oldCatalogCode, String oldCategoryCode,
      String newCatgeoryCode, List<String> productSkus) throws Exception {
    GdnBaseRestResponse response = xProductFeign
        .moveProductSalesCatalog(storeId, requestId, oldCatalogCode, oldCategoryCode, newCatgeoryCode,
            new SimpleListStringRequest(productSkus));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public Page<ProductCenterHistoryWebResponse> getProductCenterHistoryByProductSku(String productSku, int page,
      int size) {
    GdnPreconditions.checkArgument(
        Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY),
        ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    GdnRestListResponse<ProductCenterHistoryResponse> productCenterHistoryResponseList =
        xProductFeign.getProductCenterHistory(productSku, page, size);
    ResponseHelper.validateResponse(productCenterHistoryResponseList);
    List<ProductCenterHistoryWebResponse> productCenterHistoryWebResponseList =
        Optional.ofNullable(productCenterHistoryResponseList.getContent()).orElse(new ArrayList<>()).stream().map(
            productCenterHistoryResponse -> ResponseHelper
                .toProductCenterHistoryWebResponse(productCenterHistoryResponse)).collect(Collectors.toList());
    return new PageImpl<>(productCenterHistoryWebResponseList, PageRequest.of(page, size),
        productCenterHistoryResponseList.getPageMetaData().getTotalRecords());
  }
  @Override
  public ProductCenterDetailWebResponse getProductCenterDetail(String storeId, String requestId, String productSku) {
    if (Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY)) {
      GdnRestSingleResponse<ProductL3Response> productL3Response =
          xProductFeign.getProductDetailsByProductSku(storeId, requestId, productSku);
      ResponseHelper.validateResponse(productL3Response);
      CategoryMultipleIdRequest categoryRequest = new CategoryMultipleIdRequest();
      String masterCategory = StringUtils.EMPTY;
      if (Objects.nonNull(productL3Response.getValue().getMasterCatalog())){
        masterCategory = productL3Response.getValue().getMasterCatalog().getCategory().getCategoryCode();
        categoryRequest.setCategoryCode(Collections.singletonList(masterCategory));
      }
      GdnRestSingleResponse<CategoryNamesResponse> categoryNameResponse =
          pcbFeign.getCategoryNames(categoryRequest, 0, 1);
      ResponseHelper.validateResponse(categoryNameResponse);
      return ResponseHelper
          .toProductCenterDetail(productL3Response.getValue(), categoryNameResponse.getValue(), masterCategory);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  @Override
  public void updateProductCenterDetail(String storeId, String requestId, String productSku,
      SalesCategoryMappingWebRequest request) throws Exception {
    if (Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY)) {
      GdnBaseRestResponse response = xProductFeign.updateSalesCategory(storeId, requestId, productSku,
          RequestHelper.toSalesCategoryMappingUpdateRequest(request));
      ResponseHelper.validateResponse(response);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }
}