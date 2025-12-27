package com.gdn.mta.bulk.service.download;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterSkuReviewDownloadItemsRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemsDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.MasterSkuItemsDownloadResponse;
import com.gdn.mta.bulk.repository.MasterSkuItemsRepository;
import com.gdn.mta.bulk.util.RequestHelper;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkDownloadMasterSkuReviewItemsServiceBean implements BulkProcessDataService {

  @Autowired
  private MasterSkuItemsRepository masterSkuItemsRepository;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    log.info("Calling Get Data to fetch items for master sku review with requestId {}, BulkProcessEntity {}",
        request.getRequestId(), request.getBulkProcessEntity());
    MasterSkuReviewDownloadItemsRequest downloadItemsRequest = (MasterSkuReviewDownloadItemsRequest) request;
    ItemsDownloadResponse response =
        masterSkuItemsRepository.getItemsForMasterSkuReview(RequestHelper.toDownloadItemsRequest(downloadItemsRequest));
    MasterSkuItemsDownloadResponse masterSkuItemsDownloadResponse = new MasterSkuItemsDownloadResponse();
    masterSkuItemsDownloadResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    masterSkuItemsDownloadResponse.setItemSkuAndMasterSkuResponseList(response.getItemSkuAndMasterSkuResponseList());
    return masterSkuItemsDownloadResponse;
  }
}
