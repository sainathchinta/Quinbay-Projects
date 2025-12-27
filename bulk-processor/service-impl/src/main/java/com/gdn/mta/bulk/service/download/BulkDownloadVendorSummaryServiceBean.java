package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.List;


import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.VendorSummaryDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkVendorSummaryResponse;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.util.RequestConverterUtil;
import com.gdn.partners.bulk.util.Constant;


/**
 * Created by shivam on 08/07/2019 AD.
 */

@Service(value = "bulkVendorFilteredProductServiceBean")
public class BulkDownloadVendorSummaryServiceBean implements BulkProcessDataService{

  private static final int XLSX_SHEET_LIMIT = 1048576;

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkDownloadVendorSummaryServiceBean.class);

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    LOGGER.info("Calling Get Data for Vendor Filtered Product data for requestId {}, BulkProcessEntity {}",
        request.getRequestId(), request.getBulkProcessEntity());

    VendorSummaryDownloadRequest vendorSummaryDownloadRequest = (VendorSummaryDownloadRequest) request;
    List<DistributionProductResponse> responseList = new ArrayList<>();
    int vendorBulkDownloadNewSize = getSystemParameter(SystemParameterConfigNames.VENDOR_BULK_DOWNLOAD_NEW_SIZE);
    int vendorBulkDownloadMaxLimit = vendorSummaryDownloadRequest.isUnrestrictedDownload() ?
        XLSX_SHEET_LIMIT :
        getSystemParameter(SystemParameterConfigNames.VENDOR_BULK_DOWNLOAD_MAX_LIMIT);
    int pageNumber = 0;
    long totalCount;

    do {
      GdnRestListResponse<DistributionProductResponse> pageResponse =
          getResponseFromPDT(pageNumber, vendorBulkDownloadNewSize, vendorSummaryDownloadRequest);
      totalCount = pageResponse.getPageMetaData().getTotalRecords();
      responseList.addAll(pageResponse.getContent());
      ++pageNumber;
    } while (checkTotalCountAndMaxLimit(pageNumber, totalCount, vendorBulkDownloadNewSize, vendorBulkDownloadMaxLimit));

    if (checkRemainingProducts(pageNumber, totalCount, vendorBulkDownloadNewSize, vendorBulkDownloadMaxLimit)) {
      int remainingProductCount = vendorBulkDownloadMaxLimit - (pageNumber * vendorBulkDownloadNewSize);
      GdnRestListResponse<DistributionProductResponse> pageResponse =
          getResponseFromPDT(pageNumber, remainingProductCount, vendorSummaryDownloadRequest);
      responseList.addAll(pageResponse.getContent());
    }

    LOGGER.info("Total number of rows: " + responseList.size());

    BulkVendorSummaryResponse bulkVendorSummaryResponse = new BulkVendorSummaryResponse();
    bulkVendorSummaryResponse.setResponseList(responseList);
    bulkVendorSummaryResponse.setBulkProcessEntity(request.getBulkProcessEntity());
    return bulkVendorSummaryResponse;
  }

  private GdnRestListResponse<DistributionProductResponse> getResponseFromPDT(int pageNumber,
      int vendorBulkDownloadNewSize, VendorSummaryDownloadRequest vendorSummaryDownloadRequest) throws Exception {
    Pageable pageable = PageRequest.of(pageNumber, vendorBulkDownloadNewSize);
    GdnRestListResponse<DistributionProductResponse> pageResponse = productDistributionTaskRepository
        .getVendorFilteredProducts(vendorSummaryDownloadRequest.getUsername(),
            vendorSummaryDownloadRequest.getRequestId(), pageable,
            RequestConverterUtil.fromVendorSummaryDownloadRequestToFilterSummaryRequest(vendorSummaryDownloadRequest));
    return pageResponse;
  }

  private boolean checkTotalCountAndMaxLimit(int pageNumber, long totalCount, int vendorBulkDownloadNewSize,
      int vendorBulkDownloadMaxLimit) {
    return (pageNumber * vendorBulkDownloadNewSize < totalCount) && (
        vendorBulkDownloadMaxLimit - (pageNumber * vendorBulkDownloadNewSize) >= vendorBulkDownloadNewSize);
  }

  private boolean checkRemainingProducts(int pageNumber, long totalCount, int vendorBulkDownloadNewSize,
      int vendorBulkDownloadMaxLimit) {
    return ((pageNumber * vendorBulkDownloadNewSize) < totalCount) && ((pageNumber * vendorBulkDownloadNewSize)
        < vendorBulkDownloadMaxLimit);
  }

  private int getSystemParameter(String variable) {
    return Integer
        .valueOf(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, variable).getValue());
  }

}
