package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.google.common.collect.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.StoreCopyDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.StoreCopyProductResponse;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;

import lombok.extern.slf4j.Slf4j;

@Service(value = "bulkStoreCopyProductsServiceBean")
@Slf4j
public class BulkStoreCopyProductsServiceBean implements BulkProcessDataService {
  private static final int MAX_PRODUCT_SIZE = 50;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private BulkProductDataServiceBean bulkProductDataServiceBean;

  @Value("${max.L4.download.size.for.store.copy}")
  private int maxL4SizeForStoreCopy;

  @Value("${store.copy.new.flow.enabled}")
  private boolean storeCopyNewFlowEnabled;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    StoreCopyDownloadRequest storeCopyDownloadRequest = (StoreCopyDownloadRequest) request;
    ProductLevel3SummaryRequest productSummaryRequest = new ProductLevel3SummaryRequest();
    productSummaryRequest.setArchived(storeCopyDownloadRequest.getArchived());
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response =
        getActiveProductsByMerchantCode(productSummaryRequest, MAX_PRODUCT_SIZE,
            storeCopyDownloadRequest.getMerchantId());
    return new StoreCopyProductResponse(bulkDownloadProductLevel3Response.getProductLevel3SummaryResponses());
  }

  private BulkDownloadProductLevel3Response getActiveProductsByMerchantCode(
      ProductLevel3SummaryRequest productLevel3SummaryRequest, int maxProductSize, String businessPartnerCode)
      throws ApplicationException {
    int pageNumber = 0;
    List<ProductLevel3SummaryResponse> productSummaryResponseList = new ArrayList<>();
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response();
    Map<String, String> exceptionMap = new HashMap<>();
    Pageable pageable = PageRequest.of(pageNumber, maxProductSize);
    int totalRecords = 0;
    // We fetch total count in first call , if this fails download will aborted.
    if(storeCopyNewFlowEnabled) {
      ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
      productSummaryRequest.setArchived(productLevel3SummaryRequest.getArchived());
      productSummaryRequest.setMerchantCode(businessPartnerCode);

      List<ProductL3SummaryResponse> productL3SummaryResponses =
          bulkProductDataServiceBean.fetchListOfProductSkusForExcel(productSummaryRequest,
              GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername());
      List<String> productSkuList =
          productL3SummaryResponses.stream().map(ProductL3SummaryResponse::getProductSku).toList();

      for (List<String> productSkuBatch : Lists.partition(productSkuList, maxProductSize)) {
        ProductLevel3SummaryRequest productLevel3SummaryRequestDb =
            new ProductLevel3SummaryRequest();
        productLevel3SummaryRequestDb.setProductSkuList(productSkuBatch);
        productLevel3SummaryRequestDb.setPickupPointCodes(
            productSummaryRequest.getPickupPointCodes());
        productLevel3SummaryRequestDb.setPromoTypes(productSummaryRequest.getPromoTypes());

        bulkDownloadProductLevel3Response =
            productBusinessPartnerRepository.bulkDownloadSummary(productLevel3SummaryRequestDb,
                businessPartnerCode, Constants.ALL);
        productSummaryResponseList.addAll(
            bulkDownloadProductLevel3Response.getProductLevel3SummaryResponses());
        exceptionMap.putAll(bulkDownloadProductLevel3Response.getExceptionMap());

        if (productSummaryResponseList.size() > maxL4SizeForStoreCopy) {
          productSummaryResponseList = productSummaryResponseList.subList(0, maxL4SizeForStoreCopy);
          bulkDownloadProductLevel3Response =
              new BulkDownloadProductLevel3Response(productSummaryResponseList, exceptionMap);
          return bulkDownloadProductLevel3Response;
        }
      }
    } else {
      try {
        BulkDownloadProductLevel3Response bulkDownload =
            getBulkDownloadProductLevel3Response(businessPartnerCode, pageable, productLevel3SummaryRequest,
                productSummaryResponseList, bulkDownloadProductLevel3Response, exceptionMap);
        totalRecords = (int) bulkDownload.getPageMetaData().getTotalRecords();
        pageNumber++;
      } catch (Exception e) {
        log.error("Error while getting count and fetch first page businessPartnerCode : {} ", businessPartnerCode, e);
        throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS);
      }
      while (pageNumber * MAX_PRODUCT_SIZE < totalRecords) {
        try {
          getBulkDownloadProductLevel3Response(businessPartnerCode, PageRequest.of(pageNumber, maxProductSize),
              productLevel3SummaryRequest, productSummaryResponseList, bulkDownloadProductLevel3Response, exceptionMap);
        } catch (Exception e) {
          log.error("error invoking product summary from product service client. businessPartnerCode: {},"
              + " page: {}, size: {}", businessPartnerCode, pageNumber, maxProductSize, e);
        }
        pageNumber++;
      }
    }
    bulkDownloadProductLevel3Response.setProductLevel3SummaryResponses(productSummaryResponseList);
    bulkDownloadProductLevel3Response.setExceptionMap(exceptionMap);
    return bulkDownloadProductLevel3Response;
  }

  private BulkDownloadProductLevel3Response getBulkDownloadProductLevel3Response(String businessPartnerCode,
      Pageable pageable, ProductLevel3SummaryRequest productLevel3SummaryRequest,
      List<ProductLevel3SummaryResponse> productSummaryResponseList,
      BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response, Map<String, String> exceptionMap)
      throws Exception {
    BulkDownloadProductLevel3Response bulkDownload =
        productLevel3Repository.findSummaryByFilterForBulkDownload(businessPartnerCode, pageable,
            productLevel3SummaryRequest);
    productSummaryResponseList.addAll(bulkDownload.getProductLevel3SummaryResponses());
    exceptionMap.putAll(bulkDownloadProductLevel3Response.getExceptionMap());
    return bulkDownload;
  }
}

