package com.gdn.mta.bulk.repository;

import java.util.List;

import com.gdn.mta.bulk.models.download.MasterProductDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterSelectedProductDownloadRequest;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;

public interface ActiveProductRepository {

  /**
   * Get active product details from pcb
   *
   * @param masterProductDownloadRequest
   * @return
   * @throws Exception
   */
   List<MasterProductResponse> getActiveProductDetails(MasterProductDownloadRequest masterProductDownloadRequest)
       throws Exception;

  /**
   * Get active product details from pcb for selected products
   *
   * @param request
   * @return
   * @throws Exception
   */
  List<MasterProductResponse> getActiveProductDetailsForSelectedDownload(MasterSelectedProductDownloadRequest request)
      throws Exception;
}
