package com.gdn.mta.bulk.service;

import java.util.List;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.models.MasterWarehouseResponse;
import com.gdn.mta.bulk.models.SimpleListAssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.TransferRequest;

public interface ProductAssemblyOutboundService {


  /**
   * API to warehouse code and name list
   *
   * @param storeId must not be null
   * @param page    String
   * @param limit   String
   * @return List of MasterWarehouseResponse
   */
  List<MasterWarehouseResponse> getWarehouseCodeAndFulfillmentCenter(String storeId, String page, String limit);

  /**
   * Create assembly disassembly request
   *
   * @param storeId                              must not be null
   * @param type                                 must not be null
   * @param requestId                            String
   * @param username                             String
   * @param simpleListAssemblyDisassemblyRequest must not be null
   * @return GdnBaseRestResponse
   */
  GdnBaseRestResponse assemblyDisassemblyRequest(String storeId, String type, String requestId, String username,
      SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest);

  /**
   * Create transfer request
   *
   * @param storeId         must not be null
   * @param requestId       String
   * @param username        String
   * @param transferRequest must not be null
   * @return GdnBaseRestResponse
   */
  GdnBaseRestResponse transferRequest(String storeId, String requestId, String username,
      TransferRequest transferRequest);
}
