package com.gdn.x.product.service.api.migration;

import java.util.List;

public interface MigrationService {

  void fixAttributeData(String requestId, String fileName);

  void generateSpecificationDetail(String storeId, List<String> productSkus);

  void migrateProductData(String fileName, int concurrentNumber) throws Exception;

  void migrateTicketTemplate() throws Exception;
}
