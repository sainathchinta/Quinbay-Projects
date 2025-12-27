package com.gdn.partners.pbp.workflow.product;

public interface ProductWipAutoRejectService {
  
  void autoRejectProductWipNeedCorrectionExpired(String storeId) throws Exception;

}
