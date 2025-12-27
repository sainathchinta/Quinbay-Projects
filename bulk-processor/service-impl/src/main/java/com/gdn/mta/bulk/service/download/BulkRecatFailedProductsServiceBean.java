package com.gdn.mta.bulk.service.download;

import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.entity.ProductRecatStatus;
import com.gdn.mta.bulk.models.RecatFailedProducts;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.RecatFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.RecatFailedProductResponse;
import com.gdn.mta.bulk.repository.ProductRecatStatusRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.RecatConstants;

@Service(value = "bulkRecatFailedProductsServiceBean")
public class BulkRecatFailedProductsServiceBean implements BulkProcessDataService {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkRecatFailedProductsServiceBean.class);

  @Autowired
  ProductRecatStatusRepository productRecatStatusRepository;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    RecatFailedProductsDownloadRequest recatFailedProductsDownloadRequest =
        (RecatFailedProductsDownloadRequest) request;
    LOGGER.info("getting failed products data for recat-request-code {}",
        recatFailedProductsDownloadRequest.getRecatRequestCode());
    List<ProductRecatStatus> productRecatStatusList = productRecatStatusRepository
        .findByStoreIdAndStatusAndRecatRequestCode(Constant.STORE_ID,
            RecatConstants.FAILED, recatFailedProductsDownloadRequest.getRecatRequestCode());
    List<RecatFailedProducts> recatFailedProducts = productRecatStatusList.stream()
        .map(p -> new RecatFailedProducts(p.getProductCode(), p.getProductName(),
            p.getCategoryCode(), p.getCategoryName(), p.getNewCategoryCode(), p.getNewCategoryName(),
            p.isValidationError() ? p.getErrorMessage() : "" )).collect(Collectors.toList());
    RecatFailedProductResponse recatFailedProductResponse = new RecatFailedProductResponse();
    recatFailedProductResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    recatFailedProductResponse.setResponseList(recatFailedProducts);
    return recatFailedProductResponse;
  }
}
