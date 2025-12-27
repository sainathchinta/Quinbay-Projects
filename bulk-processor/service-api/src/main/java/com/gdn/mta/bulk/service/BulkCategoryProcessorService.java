package com.gdn.mta.bulk.service;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.entity.BulkProcess;

import java.util.List;
import java.util.Map;

public interface BulkCategoryProcessorService {

  /**
   * Api to process bulk generic event
   *
   * @param bulkCreateProductEventModel
   * @return
   * @throws Exception
   */
  void processEvent(BulkCreateProductEventModel bulkCreateProductEventModel) throws Exception;

  /**
   * Api to save the data and image URLs for generic bulk process
   *
   * @param bulkProcess
   * @param userInputRows
   * @param headers
   * @param accessiblePickupPoints
   * @return
   * @throws Exception
   */
  void generateBulkProcessDataAndImage(BulkProcess bulkProcess, List<List<Object>> userInputRows, List<Object> headers,
      String accessiblePickupPoints) throws Exception;

  /**
   *
   * @param groupRaw
   * @param productCreationRequest
   * @param imageUrlAndLocationMap
   * @return
   */
  Map<String, String> generateImageFilename(List<Map<String, Object>> groupRaw,
      ProductCreationRequest productCreationRequest, Map<String, String> imageUrlAndLocationMap);
}
