package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.ItemBasicL4Response;
import com.gdn.mta.bulk.helper.BulkEANHelper;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductUPCDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductLevel4Response;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductEANResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductLiteResponse;
import com.gdn.mta.bulk.models.download.responsedata.PartialAllDownloadResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.google.common.collect.Lists;


@Service(value = "bulkProductUPCDataServiceBean")
public class BulkProductUPCDataServiceBean implements BulkProcessDataService {
  private static final Logger LOGGER = LoggerFactory.getLogger(BulkProductUPCDataServiceBean.class);
  private static final String STORE_ID = "10001";

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${max.product.fetch.size.ean}")
  private int maxProductSize;

  @Value("${max.download.size.ean}")
  private int maxL4DownloadSize;

  @Value("${batch.size.l4.ean}")
  private int batchSizeL4;

  @Value("${product.sku.size.ean}")
  private int productSkuSize;


  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    LOGGER.info("Calling Get Data for Product for request {}", request);
    ProductUPCDownloadRequest productRequest = objectMapper.convertValue(request, ProductUPCDownloadRequest.class);
    ProfileResponse businessPartner =
        businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, request.getMerchantId());
    PartialAllDownloadResponse partialAllDownloadResponse = new PartialAllDownloadResponse();
    BulkDownloadProductLevel4Response result = getProductsFromProductService(businessPartner.getBusinessPartnerCode(),
        productRequest.getProductSummaryRequest(), productRequest.getUsername(), productRequest.getRequestId(),
        partialAllDownloadResponse);
    List<List<String>> productContentsForXLFile = BulkEANHelper.getAllProductValues(result.getBulkProductEANResponse());


    if (CollectionUtils.isEmpty(productContentsForXLFile)) {
      LOGGER.error("Bulk Download: No Data Found for Bulk Download product for request {} and business " + "partner {}",
          request.getRequestId(), request.getMerchantId());
    }
    request.setExceptionMsg(StringUtils.EMPTY);
    return new BulkProductLiteResponse(productContentsForXLFile, partialAllDownloadResponse.isPartialDownload());
  }

  private BulkDownloadProductLevel4Response getProductsFromProductService(String businessPartnerCode,
      ProductSummaryRequest productSummaryRequest, String username, String requestId,
      PartialAllDownloadResponse partialAllDownloadResponse) {
    List<ProductL3SummaryResponse> productL3SummaryResponses =
        fetchCompleteProductL3SummaryList(productSummaryRequest, requestId, username);
    List<String> productSkuList = new ArrayList<>();
    Map<String, String> productSkuAndNameMap = new HashMap<>();
    mapSkuToProductName(productL3SummaryResponses, productSkuList, productSkuAndNameMap);
    List<BulkProductEANResponse> allItems =
        fetchL4Items(productSkuList, requestId, username, productSkuAndNameMap, partialAllDownloadResponse,
            businessPartnerCode);
    return new BulkDownloadProductLevel4Response(allItems);
  }

  private List<BulkProductEANResponse> fetchL4Items(List<String> productSkuList, String requestId, String username,
      Map<String, String> productSkuAndNameMap, PartialAllDownloadResponse partialAllDownloadResponse,
      String businessPartnerCode) {
    List<BulkProductEANResponse> allItems = new ArrayList<>();
    for (List<String> productSkus : Lists.partition(productSkuList, productSkuSize)) {
      int pageNumber = 0;
      Page<ItemBasicL4Response> xProductResponse = new PageImpl<>(new ArrayList<>());
      ItemLevel4ListingWebRequest request = new ItemLevel4ListingWebRequest();
      request.setProductSkus(productSkus.stream().filter(Objects::nonNull).collect(Collectors.toSet()));
      do {
        try {
          xProductResponse =
              xProductOutboundService.getL4ItemListByProductSku(pageNumber, batchSizeL4, requestId, username, request);
          for (ItemBasicL4Response itemBasicL4Response : xProductResponse.getContent()) {
            addMappedItemToList(itemBasicL4Response, productSkuAndNameMap, allItems);
            if (allItems.size() >= maxL4DownloadSize) {
              partialAllDownloadResponse.setPartialDownload(true);
              return new ArrayList<>(allItems.subList(0, maxL4DownloadSize));
            }
          }
          LOGGER.info("Total L4 items fetched for SKU {} = {}", productSkus, allItems.size());
        } catch (Exception e) {
          LOGGER.error(
              "error invoking product summary from product service client. businessPartnerCode: {}," + " page: {}",
              businessPartnerCode, pageNumber, e);
        }
        pageNumber++;
      } while (((long) pageNumber * batchSizeL4) < Objects.requireNonNull(xProductResponse).getTotalElements());


    }
    return allItems;
  }

  private static void addMappedItemToList(ItemBasicL4Response itemBasicL4Response,
      Map<String, String> productSkuAndNameMap, List<BulkProductEANResponse> allItems) {
    BulkProductEANResponse level4Response =  BulkProductEANResponse.builder()
        .upcCode(itemBasicL4Response.getUpcCode())
        .productSku(itemBasicL4Response.getProductSku())
        .itemSku(itemBasicL4Response.getItemSku())
        .productName(productSkuAndNameMap.get(itemBasicL4Response.getProductSku()))
        .itemName(itemBasicL4Response.getItemName())
        .build();
    allItems.add(level4Response);
  }

  private static void mapSkuToProductName(List<ProductL3SummaryResponse> responses, List<String> productSkuList,
      Map<String, String> productSkuAndNameMap) {
    responses.forEach(response -> {
      productSkuAndNameMap.put(response.getProductSku(), response.getProductName());
      productSkuList.add(response.getProductSku());
    });
  }

  private List<ProductL3SummaryResponse> fetchCompleteProductL3SummaryList(ProductSummaryRequest productSummaryRequest,
      String requestId, String username) {
    int pageNumber = 0;
    Page<ProductL3SummaryResponse> productL3SummaryResponsePage = new PageImpl<>(new ArrayList<>());
    List<ProductL3SummaryResponse> productL3SummaryResponseList = new ArrayList<>();
    do {
      try {
        productL3SummaryResponsePage =
            this.xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, pageNumber, maxProductSize,
                requestId, username);
        productL3SummaryResponseList.addAll(productL3SummaryResponsePage.getContent());
      } catch (Exception e) {
        LOGGER.error("Error fetching product L3 list for request : {}, page: {}, size: {}", productSummaryRequest,
            pageNumber, maxProductSize, e);
      }
      pageNumber++;
    } while (((long) pageNumber * maxProductSize) < productL3SummaryResponsePage.getTotalElements());
    return productL3SummaryResponseList;
  }


}
