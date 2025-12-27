package com.gdn.mta.bulk.service.download;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterSelectedProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterProductResponse;
import com.gdn.mta.bulk.repository.ActiveProductRepository;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;

@Service(value = "bulkSelectedMasterProductDataServiceBean")
public class BulkSelectedMasterProductsDownloadServiceBean implements BulkProcessDataService {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkSelectedMasterProductsDownloadServiceBean.class);

  @Autowired
  private ActiveProductRepository activeProductRepository;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    LOGGER.info("Calling Get Data for master product data for requestId {}", request.getRequestId());
    MasterSelectedProductDownloadRequest masterSelectedProductDownloadRequest =
        (MasterSelectedProductDownloadRequest) request;
    List<MasterProductResponse> masterProductResponseList =
        activeProductRepository.getActiveProductDetailsForSelectedDownload(masterSelectedProductDownloadRequest);
    BulkMasterProductResponse bulkMasterProductResponse = new BulkMasterProductResponse();
    bulkMasterProductResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    bulkMasterProductResponse.setResponseList(masterProductResponseList);
    return bulkMasterProductResponse;
  }
}
