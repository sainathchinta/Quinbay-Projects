package com.gdn.mta.bulk.service.download;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.PartialAllDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.PickupPointModel;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.service.PickupPointService;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.google.common.collect.Lists;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Created by keshashah on 25/10/16.
 */
@Service(value = "bulkProductDataServiceBean")
public class BulkProductDataServiceBean implements BulkProcessDataService {
  private static final Logger LOGGER = LoggerFactory.getLogger(BulkProductDataServiceBean.class);
  private static final String STORE_ID = "10001";
  private static final Object INVENTORY_FULFILLMENT_BLIBLI = "BL";
  private static final Integer MAX_COUNT = 10;
  private static final String LOG_PREFIX = "LOG_PREFIX";
  private static final String LOG_SUFFIX = "LOG_SUFFIX";
  private static final String AND = "AND";
  private static final String MORE = "MORE";

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private PickupPointService pickupPointService;

  @Value("${product.download.fetch.from.db}")
  private boolean productDownloadFetchFromDb;

  @Value("${max.L3.product.fetch.size}")
  private int maxProductSize;

  @Value("${max.L5.download.size}")
  private int maxL5DownloadSize;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    LOGGER.info("Calling Get Data for Product for request {}", request);
    ProductDownloadRequest productRequest = (ProductDownloadRequest) request;
    ProfileResponse businessPartner = businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID,
            request.getMerchantId());
    List<PickupPointResponse> pickupPointResponseList = this.pickupPointService.getPickupPointSummaryFilter(0,
        PickupPointFilterRequest.builder().businessPartnerCode(request.getMerchantId())
            .codes(productRequest.getAccessiblePickupPoints()).build());
    boolean isBusinessPartnerO2O = businessPartner.getCompany().isOfflineToOnlineFlag();
    boolean isBlibliFulfillment =
        INVENTORY_FULFILLMENT_BLIBLI.equals(businessPartner.getCompany().getInventoryFulfillment());

    PartialAllDownloadResponse partialAllDownloadResponse = new PartialAllDownloadResponse();
    BulkDownloadProductLevel3Response result = getProductsFromProductService(businessPartner.getBusinessPartnerCode(),
        productRequest.getProductSummaryRequest(), productRequest.getUsername(), productRequest.getRequestId(),
        partialAllDownloadResponse);
    List<PickupPointModel> pickupPointModelList = getPickupPoints(pickupPointResponseList);
    List<List<String>> productContentsForXLFile = bulkDownloadServiceBeanUtil
        .getAllProductValues(productRequest.getPrivilegedMap(), isBusinessPartnerO2O,
            result.getProductLevel3SummaryResponses(), isBlibliFulfillment, businessPartner, pickupPointModelList);
    Map<String, String> exceptionMap = result.getExceptionMap();
    if (CollectionUtils.isEmpty(productContentsForXLFile)) {
      LOGGER.error(
          "Bulk Download: No Data Found for Bulk Download product for request {} and business "
              + "partner {}", request.getRequestId(), request.getMerchantId());
    }
    request.setExceptionMsg(StringUtils.EMPTY);
    if(!MapUtils.isEmpty(exceptionMap)) {
      String exceptionString = getExceptionMsgString(exceptionMap, productRequest.getProductSize(), request.getLanguage());
      request.setExceptionMsg(exceptionString);
    }
    return new BulkProductResponse(productContentsForXLFile, isBusinessPartnerO2O,
        ((ProductDownloadRequest) request).getPrivilegedMap(), pickupPointModelList, exceptionMap,
        partialAllDownloadResponse.isPartialDownload());
  }

  private List<PickupPointModel> getPickupPoints(
    List<PickupPointResponse> pickupPointResponseList) {
    List<PickupPointModel> pickupPointModelList = new ArrayList<>();
    pickupPointResponseList.forEach(pickupPointResponse -> {
      pickupPointModelList.add(new PickupPointModel(pickupPointResponse.getName(),
        pickupPointResponse.getCode(), pickupPointResponse.isFbbActivated()));
    });
    return pickupPointModelList;
  }

  private BulkDownloadProductLevel3Response getProductsFromProductService(String businessPartnerCode,
      ProductSummaryRequest productSummaryRequest, String username, String requestId,
      PartialAllDownloadResponse partialAllDownloadResponse) throws Exception {
    List<ProductLevel3SummaryResponse> productSummaryResponseList = new ArrayList<>();
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response();
    Map<String, String> exceptionMap = new HashMap<>();
    List<ProductL3SummaryResponse> productL3SummaryResponses =
        fetchListOfProductSkusForExcel(productSummaryRequest, requestId, username);

    List<String> productSkuList =
        productL3SummaryResponses.stream().map(ProductL3SummaryResponse::getProductSku).collect(Collectors.toList());
    Map<String, String> productSkuAndNameMap = new HashMap<>();
    if (productDownloadFetchFromDb) {
      if (CollectionUtils.isNotEmpty(productL3SummaryResponses)) {
        productSkuAndNameMap = productL3SummaryResponses.stream()
          .filter(summaryResponse -> StringUtils.isNotEmpty(summaryResponse.getProductName()))
          .collect(Collectors.toMap(ProductL3SummaryResponse::getProductSku, ProductL3SummaryResponse::getProductName, (a, b) -> b));
      }
    }
    for (List<String> productSkuBatch : Lists.partition(productSkuList, maxProductSize)) {
      int pageNumber = 0;
      ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
      productLevel3SummaryRequest.setProductSkuList(productSkuBatch);
      productLevel3SummaryRequest.setPickupPointCodes(productSummaryRequest.getPickupPointCodes());
      productLevel3SummaryRequest.setPromoTypes(productSummaryRequest.getPromoTypes());
      do {
        try {
          Pageable pageable = PageRequest.of(pageNumber, maxProductSize);
          if(!productDownloadFetchFromDb) {
            bulkDownloadProductLevel3Response = productLevel3Repository.findSummaryByFilterForBulkDownload(businessPartnerCode, pageable,
                productLevel3SummaryRequest);
          } else {
             bulkDownloadProductLevel3Response = productBusinessPartnerRepository.bulkDownloadSummary(productLevel3SummaryRequest, businessPartnerCode,
                 Constants.ALL);
          }
          if (Objects.isNull(bulkDownloadProductLevel3Response) || (
              CollectionUtils.isEmpty(bulkDownloadProductLevel3Response.getProductLevel3SummaryResponses()) && MapUtils
                  .isEmpty(bulkDownloadProductLevel3Response.getExceptionMap()))) {
            pageNumber++;
            continue;
          }
          if (productDownloadFetchFromDb) {
            for (ProductLevel3SummaryResponse productLevel3SummaryResponse : bulkDownloadProductLevel3Response.getProductLevel3SummaryResponses()) {
              productLevel3SummaryResponse.setProductName(
                  productSkuAndNameMap.get(productLevel3SummaryResponse.getProductSku()));
            }
          }
          productSummaryResponseList.addAll(bulkDownloadProductLevel3Response.getProductLevel3SummaryResponses());
          if (productSummaryResponseList.size() > maxL5DownloadSize) {
            partialAllDownloadResponse.setPartialDownload(true);
            productSummaryResponseList = productSummaryResponseList.subList(0, maxL5DownloadSize);
            bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response(productSummaryResponseList, exceptionMap);
            return bulkDownloadProductLevel3Response;
          }
          exceptionMap.putAll(bulkDownloadProductLevel3Response.getExceptionMap());
        } catch (Exception e) {
          LOGGER.error(
              "error invoking product summary from product service client. businessPartnerCode: {},"
                  + " page: {}, size: {}", businessPartnerCode, pageNumber, maxProductSize, e);
        }
        pageNumber++;
      } while (Objects.nonNull(bulkDownloadProductLevel3Response) && pageNumber < Math
          .ceil((double) bulkDownloadProductLevel3Response.getPageMetaData().getTotalRecords() / maxProductSize));
    }
    bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response(productSummaryResponseList, exceptionMap);
    return bulkDownloadProductLevel3Response;
  }

  public List<ProductL3SummaryResponse> fetchListOfProductSkusForExcel(ProductSummaryRequest productSummaryRequest,
      String requestId, String username) {
    int pageNumber = 0;
    Page<ProductL3SummaryResponse> productL3SummaryResponsePage = new PageImpl<>(new ArrayList<>());
    List<ProductL3SummaryResponse> productL3SummaryResponseList = new ArrayList<>();
    do {
      try {
        productL3SummaryResponsePage = this.xProductOutboundService
          .getProductL3SummaryResponse(productSummaryRequest, pageNumber, maxProductSize, requestId,
            username);
        productL3SummaryResponseList.addAll(productL3SummaryResponsePage.getContent());
        pageNumber++;
      } catch (Exception e) {
        LOGGER.error("Error fetching product L3 list for request : {}, page: {}, size: {}",
          productSummaryRequest, pageNumber, maxProductSize, e);
        pageNumber++;
      }

    } while ((pageNumber * maxProductSize) < productL3SummaryResponsePage.getTotalElements());
    return productL3SummaryResponseList;
  }

  public static String getExceptionMsgString(Map<String, String> exceptionMap, Integer productSize, String lang) {
    int exceptionCount = exceptionMap.size();

    Map<String, String> resultMap = new HashMap<>();
    StringBuilder log = new StringBuilder();
    if (exceptionCount > 0) {
      StringBuilder extraInfo = new StringBuilder();
      if (exceptionMap.size() > MAX_COUNT) {
        exceptionCount = exceptionCount - MAX_COUNT;
        extraInfo =
            extraInfo.append(" ....").append(MessageUtil.getMessage(AND, lang)).append(" ").append(exceptionCount).append(" ")
                .append(MessageUtil.getMessage(MORE, lang));
        for (Map.Entry<String, String> entry : exceptionMap.entrySet()) {
          if (resultMap.size() == MAX_COUNT) {
            break;
          }
          resultMap.put(entry.getKey(), entry.getValue());
        }
      } else {
        resultMap = exceptionMap;
      }
      StringBuilder exceptionLog = new StringBuilder();
      exceptionLog.append(MessageUtil.getMessage(LOG_PREFIX, lang)).append(resultMap.toString());
      log = exceptionLog.append(extraInfo).append(MessageUtil.getMessage(LOG_SUFFIX, lang)).append(productSize.toString());
    }
    return log.toString();
  }


}
