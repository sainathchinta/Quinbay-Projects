package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.service.PBPOutboundService;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkCampaignProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.repository.pcb.ProductCategoryBaseRepository;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.partners.bulk.util.Constant;
import com.google.common.collect.Lists;

@Service(value = "bulkCampaignProductDataServiceBean")
public class BulkCampaignProductDataServiceBean implements BulkProcessDataService {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkCampaignProductDataServiceBean.class);

  @Value("${bulk.process.getAllChildCategory.batch.size:100}")
  private int bulkGetAllChildCategorySize;

  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Autowired
  private ProductCategoryBaseRepository productCategoryBaseRepository;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    LOGGER.info("Calling get campaign product data for request {}", request);
    CampaignProductDownloadRequest campaignProductDownloadRequest = (CampaignProductDownloadRequest) request;
    List<List<ProductLevel3SummaryResponse>> responses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(campaignProductDownloadRequest.getCampaignItemSummaryRequest().getCategories())) {
      for (String categoryCode : campaignProductDownloadRequest.getCampaignItemSummaryRequest().getCategories()) {
        List<String> childCategories = productCategoryBaseRepository
            .getAllChildCategoriesFromC1CategoryCode(request.getRequestId(), null,
                Collections.singletonList(categoryCode));
        if (CollectionUtils.isEmpty(childCategories)) {
          childCategories = Collections.singletonList(categoryCode);
          campaignProductDownloadRequest.getCampaignItemSummaryRequest().setCategories(childCategories);
          getProductsForCampaign(campaignProductDownloadRequest, responses);
        } else {
          int partitionSize = Integer.parseInt(systemParameterConfigService
              .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.CATEGORY_CODE_PARTITION_SIZE).getValue());
          List<List<String>> childCategoriesPartition = Lists.partition(childCategories, partitionSize);
          for (List<String> categoryRequest : childCategoriesPartition) {
            campaignProductDownloadRequest.getCampaignItemSummaryRequest().setCategories(categoryRequest);
            getProductsForCampaign(campaignProductDownloadRequest, responses);
          }
        }
      }
    } else {
      campaignProductDownloadRequest.getCampaignItemSummaryRequest().setCategories(Collections.emptyList());
      getProductsForCampaign(campaignProductDownloadRequest, responses);
    }
    List<ProductLevel3SummaryResponse> flatResponses =
        responses.stream().flatMap(List::stream).collect(Collectors.toList());
    if (campaignProductDownloadRequest.getSize() != 0) {
      List<List<ProductLevel3SummaryResponse>> responsePartition =
          Lists.partition(flatResponses, campaignProductDownloadRequest.getSize());
      if (CollectionUtils.isNotEmpty(responsePartition))
        flatResponses = responsePartition.get(campaignProductDownloadRequest.getPage());
    }
    BulkCampaignProductResponse bulkCampaignProductResponse = new BulkCampaignProductResponse(flatResponses);
    bulkCampaignProductResponse.setPromoType(campaignProductDownloadRequest.getPromoType());
    bulkCampaignProductResponse.setRecommendedWeek(campaignProductDownloadRequest.getRecommendedWeek());
    return bulkCampaignProductResponse;
  }

  private void getProductsForCampaign(CampaignProductDownloadRequest campaignProductDownloadRequest,
    List<List<ProductLevel3SummaryResponse>> responses) throws Exception {
    if (multiPickupPointEnabled) {
      List<String> productSkuList = new ArrayList<>();
      fetchProductSkuList(campaignProductDownloadRequest, productSkuList);
      responses.add(fetchProductSummaryByProductSkuList(
        campaignProductDownloadRequest.getCampaignItemSummaryRequest(), productSkuList));
    } else {
      int index = 0;
      while (true) {
        List<ProductLevel3SummaryResponse> response =
          productLevel3Repository.findSummaryByCategoryAndBrand(index, bulkGetAllChildCategorySize,
            campaignProductDownloadRequest);
        responses.add(response);
        if (CollectionUtils.isEmpty(response)) {
          break;
        }
        index++;
      }
    }
  }

  private List<ProductLevel3SummaryResponse> fetchProductSummaryByProductSkuList(
    CampaignItemSummaryRequest campaignItemSummaryRequest, List<String> productSkuList)
    throws ApplicationException {
    int productPartitionSize =
      Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
      SystemParameterConfigNames.PRODUCT_PARTITION_SIZE).getValue());
    Page<ProductLevel3SummaryResponse> productLevel3SummaryResponses;
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = new ArrayList<>();
    List<List<String>> productSkuPartition = Lists.partition(productSkuList, productPartitionSize);
    for (List<String> productSkuBatch : productSkuPartition) {
      int page = 0;
      do {
        productLevel3SummaryResponses =
          pbpOutboundService.fetchProductLevel3SummaryByProductSkuList(page,
            bulkGetAllChildCategorySize, campaignItemSummaryRequest.getMerchantCode(), true,
            productSkuBatch);
        productLevel3SummaryResponseList.addAll(productLevel3SummaryResponses.getContent());
        page++;
      } while (page < Math.ceil(
        (double) productLevel3SummaryResponses.getTotalElements() / bulkGetAllChildCategorySize));
    }
    return productLevel3SummaryResponseList;
  }

  private void fetchProductSkuList(CampaignProductDownloadRequest campaignProductDownloadRequest,
    List<String> productSkuList) {
    int page = 0;
    ProductSkuSummaryRequest productSkuSummaryRequest =
      ConverterUtil.toProductSkuSummaryRequest(campaignProductDownloadRequest);
    Page<ProductSkuSummaryResponse> productSkuSummaryResponse = null;
    do {
      productSkuSummaryResponse = xProductOutboundService.getProductSkuSummaryResponse(
        campaignProductDownloadRequest.getCampaignItemSummaryRequest().getMerchantCode(), page,
        bulkGetAllChildCategorySize, productSkuSummaryRequest);
      productSkuList.addAll(
        Optional.ofNullable(productSkuSummaryResponse.getContent()).orElse(new ArrayList<>())
          .stream().map(ProductSkuSummaryResponse::getProductSku).collect(Collectors.toList()));
      page++;
    } while (page < Math.ceil(
      (double) productSkuSummaryResponse.getTotalElements() / bulkGetAllChildCategorySize));
  }
}
