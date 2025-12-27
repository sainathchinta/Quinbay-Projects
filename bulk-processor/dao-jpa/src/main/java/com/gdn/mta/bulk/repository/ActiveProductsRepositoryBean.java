package com.gdn.mta.bulk.repository;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

import com.gda.mta.product.dto.ProductCodesResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.models.download.MasterProductDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterSelectedProductDownloadRequest;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import com.google.common.collect.Lists;

@Repository
public class ActiveProductsRepositoryBean implements ActiveProductRepository {


  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private PCBFeign pcbFeign;

  private static final int MAX_PRODUCT_SIZE = 50;
  private static final Logger LOGGER = LoggerFactory.getLogger(ActiveProductsRepositoryBean.class);


  @Override
  public List<MasterProductResponse> getActiveProductDetails(MasterProductDownloadRequest masterProductDownloadRequest)
      throws Exception {
    List<MasterProductResponse> masterProductResponse = new ArrayList<>();
    GdnRestSingleResponse<ProductCodesResponse> productCodesResponse =
        this.pbpFeign.filterProductCollectionSummaryByKeywordforBulkDownload(
            GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), masterProductDownloadRequest.getCategoryCode(),
            masterProductDownloadRequest.getFilterName(), masterProductDownloadRequest.getReviewPending(),
            masterProductDownloadRequest.getSortBy(), true, true);

    ProductCodesResponse codesResponse = productCodesResponse.getValue();
    getMasterDataFromPCB(masterProductDownloadRequest.getRequestId(), masterProductDownloadRequest.getUsername(),
        masterProductResponse, codesResponse.getProductCodes());
    return masterProductResponse;
  }

  private void getMasterDataFromPCB(String requestId, String userName,
      List<MasterProductResponse> masterProductResponse, List<String> productCodesList) {
    List<List<String>> productCodesPartition = Lists.partition(productCodesList, MAX_PRODUCT_SIZE);
    for (List<String> productCodes: productCodesPartition) {
      try {
        GdnRestListResponse<MasterProductResponse> response =
            pcbFeign.getProductDetailListByProductCodesforBulkDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
                GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
                requestId, userName,
                productCodes);
        if (CollectionUtils.isEmpty(response.getContent())) {
          LOGGER.error("Bulk Download: Product Response is empty for productsCodes: {}", productCodes);
        }
        masterProductResponse.addAll(response.getContent());
      } catch (Exception e) {
        LOGGER.error("error invoking get product details from product service client. size: {}, product codes:{}",
            productCodes.size(), productCodes, e);
      }
    }
  }

  @Override
  public List<MasterProductResponse> getActiveProductDetailsForSelectedDownload(
      MasterSelectedProductDownloadRequest request) throws Exception {
    List<String> productCodes = request.getProductCodes();
    List<MasterProductResponse> masterProductResponse = new ArrayList<>();
    getMasterDataFromPCB(GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), masterProductResponse, productCodes);
    return masterProductResponse;
  }
}
