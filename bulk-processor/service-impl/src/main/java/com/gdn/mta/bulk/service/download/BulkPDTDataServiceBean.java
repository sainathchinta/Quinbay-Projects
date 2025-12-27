package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.common.web.param.PageableHelper;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ProductVendorDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductVendorResponse;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;

@Service("bulkPDTDataServiceBean")
public class BulkPDTDataServiceBean implements BulkProcessDataService {

  @Value("${vendor.bulk.download.old.size}")
  public int vendorBulkDownloadOldSize;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Override
  public BulkDataResponse getData(BulkDownloadRequest request) throws Exception {
    ProductVendorDownloadRequest productVendorDownloadRequest =
        (ProductVendorDownloadRequest) request;
    List<DistributionProductResponse> responseList = new ArrayList<>();
    int i = 0;
    Set<String> productCodes = new HashSet<>();
    Page<DistributionProductResponse> responseCount = productDistributionTaskRepository
        .getProductsForVendor(request.getRequestId(), request.getUsername(),
            PageRequest.of(0, 1),
            productVendorDownloadRequest.getProductListRequest());
    Long totalCount = responseCount.getTotalElements();
    while ((i * vendorBulkDownloadOldSize) < totalCount) {
      Pageable pageable = PageRequest.of(i, vendorBulkDownloadOldSize);
      Page<DistributionProductResponse> pageResponse = productDistributionTaskRepository
          .getProductsForVendor(request.getRequestId(), request.getUsername(), pageable,
              productVendorDownloadRequest.getProductListRequest());
      List<DistributionProductResponse> listResponse = new ArrayList<>(pageResponse.getContent());
      validateDuplicateProduct(productCodes, listResponse);
      responseList.addAll(listResponse);
      i++;
    }
    return new BulkProductVendorResponse(responseList);
  }

  private void validateDuplicateProduct(Set<String> productCodes,
      List<DistributionProductResponse> source) {
    Iterator<DistributionProductResponse> iterator = source.iterator();
    while (iterator.hasNext()) {
      DistributionProductResponse distribution = iterator.next();
      if (productCodes.contains(distribution.getProductCode())) {
        iterator.remove();
      } else {
        productCodes.add(distribution.getProductCode());
      }
    }
  }
}
