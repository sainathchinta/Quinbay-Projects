package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductBasicInfoDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductSkuResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.PCBOutboundService;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkProductBasicInfoDataServiceBean implements BulkProcessDataService {

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;


  @Value("${max.L3.product.fetch.size}")
  private int maxProductSize;


  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    log.info("Calling Get Data for Product basic info for request {}", request);
    ProductBasicInfoDownloadRequest productRequest = (ProductBasicInfoDownloadRequest) request;
    ProfileResponse businessPartner =
        businessPartnerRepository.filterByBusinessPartnerCodeV2(Constants.DEFAULT_STORE_ID, request.getMerchantId());
    boolean isBusinessPartnerO2O = businessPartner.getCompany().isOfflineToOnlineFlag();
    BulkDownloadProductBasicInfoResponse result =
        getProductsFromProductService(businessPartner.getBusinessPartnerCode(),
            productRequest.getProductSummaryRequest(), productRequest.getUsername(), productRequest.getRequestId());
    Set<String> categoryCodes = CommonUtils.populateCommonImagesAndCategoryCodes(result);
    List<CategoryHierarchyResponse> categoryHierarchyResponses =
        pcbOutboundService.filterCategoryHierarchyByCategoryCodes(new ArrayList<>(categoryCodes));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    CommonUtils.populateCategoryCodeAndHierarchyMap(categoryHierarchyResponses, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    List<List<String>> productContentsForXLFile =
        bulkDownloadServiceBeanUtil.getAllProductBasicInfoValues(isBusinessPartnerO2O, result,
            categoryCodeAndHierarchyMap, categoryCodeAndLogisticAdjustmentMap);
    Map<String, String> exceptionMap = result.getExceptionMap();
    if (CollectionUtils.isEmpty(productContentsForXLFile)) {
      log.error("Bulk Download: No Data Found for Bulk Download product for request {} and business " + "partner {}",
          request.getRequestId(), request.getMerchantId());
    }
    request.setExceptionMsg(StringUtils.EMPTY);
    if (!MapUtils.isEmpty(exceptionMap)) {
      String exceptionString =
          BulkProductDataServiceBean.getExceptionMsgString(exceptionMap, productContentsForXLFile.size(),
              request.getLanguage());
      request.setExceptionMsg(exceptionString);
    }
    BulkProductBasicInfoResponse bulkProductBasicInfoResponse = new BulkProductBasicInfoResponse();
    bulkProductBasicInfoResponse.setProductContentList(productContentsForXLFile);
    bulkProductBasicInfoResponse.setExceptionMap(exceptionMap);
    bulkProductBasicInfoResponse.setBusinessPartnerO2O(isBusinessPartnerO2O);
    bulkProductBasicInfoResponse.setPrivilegedMap(((ProductBasicInfoDownloadRequest) request).getPrivilegedMap());
    ShippingTypeEligibility shippingTypeEligibility = new ShippingTypeEligibility();
    shippingTypeEligibility.setEligibleForBopisProduct(
        BooleanUtils.toBooleanDefaultIfNull(businessPartner.getBopisFlag(), false));
    shippingTypeEligibility.setEligibleForBigProduct(
        BooleanUtils.toBooleanDefaultIfNull(businessPartner.getBigProductFlag(), false));
    bulkProductBasicInfoResponse.setShippingTypeEligibility(shippingTypeEligibility);
    return bulkProductBasicInfoResponse;
  }

  public BulkDownloadProductBasicInfoResponse getProductsFromProductService(String businessPartnerCode,
      ProductSummaryRequest productSummaryRequest, String username, String requestId) {
    List<ProductBasicInfoResponse> productBasicInfoResponseList = new ArrayList<>();
    Map<String, String> exceptionMap = new HashMap<>();
    List<ProductSkuResponse> productSkuResponseList =
        fetchListOfProductSkusForExcel(productSummaryRequest, requestId, username);
    Set<String> productSkuList =
        productSkuResponseList.stream().map(ProductSkuResponse::getProductSku).collect(Collectors.toSet());
    for (List<String> productSkuBatch : Lists.partition(new ArrayList<>(productSkuList), maxProductSize)) {
      ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
      productLevel3SummaryRequest.setProductSkuList(productSkuBatch);
      productLevel3SummaryRequest.setPickupPointCodes(productSummaryRequest.getPickupPointCodes());
      productLevel3SummaryRequest.setPromoTypes(productSummaryRequest.getPromoTypes());
      addBulkDownloadProductBasicInfoResponse(businessPartnerCode, productBasicInfoResponseList, exceptionMap,
          productLevel3SummaryRequest);
    }
    return new BulkDownloadProductBasicInfoResponse(productBasicInfoResponseList, exceptionMap);
  }

  public void addBulkDownloadProductBasicInfoResponse(String businessPartnerCode,
      List<ProductBasicInfoResponse> productBasicInfoResponseList, Map<String, String> exceptionMap,
      ProductLevel3SummaryRequest productLevel3SummaryRequest) {
    try {
      BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
          xProductOutboundService.getProductBasicInfoDetails(productLevel3SummaryRequest);
      if (!CommonUtils.isBulkDownloadResponseEmptyOrInvalid(bulkDownloadProductBasicInfoResponse)) {
        productBasicInfoResponseList.addAll(bulkDownloadProductBasicInfoResponse.getProductBasicInfoResponseList());
        exceptionMap.putAll(bulkDownloadProductBasicInfoResponse.getExceptionMap());
      }
    } catch (Exception e) {
      log.error("Error invoking product summary from product service client. businessPartnerCode: {}",
          businessPartnerCode, e);
    }
  }

  public List<ProductSkuResponse> fetchListOfProductSkusForExcel(ProductSummaryRequest productSummaryRequest,
      String requestId, String username) {
    int pageNumber = 0;
    productSummaryRequest.setArchived(false);
    productSummaryRequest.setSuspended(false);
    Page<ProductSkuResponse> productSkuResponsePage = new PageImpl<>(new ArrayList<>());
    List<ProductSkuResponse> productSkuResponseList = new ArrayList<>();
    do {
      try {
        productSkuResponsePage =
            this.xProductOutboundService.getProductSkuResponse(productSummaryRequest, pageNumber, maxProductSize,
                requestId, username);
        productSkuResponseList.addAll(productSkuResponsePage.getContent());
        pageNumber++;
      } catch (Exception e) {
        log.error("Error fetching product L3 list for request : {}, page: {}, size: {}", productSummaryRequest,
            pageNumber, maxProductSize, e);
        pageNumber++;
      }
    } while (((long) pageNumber * maxProductSize) < productSkuResponsePage.getTotalElements());
    return productSkuResponseList;
  }
}
