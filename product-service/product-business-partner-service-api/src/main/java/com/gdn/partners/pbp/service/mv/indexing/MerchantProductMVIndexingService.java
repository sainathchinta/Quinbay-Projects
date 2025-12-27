package com.gdn.partners.pbp.service.mv.indexing;

import java.util.List;

import com.gdn.common.web.param.MandatoryRequestParam;


public interface MerchantProductMVIndexingService extends MaterializedViewIndexingService {
  void partialIndexingByItemSkus(MandatoryRequestParam mandatoryRequestParam, List<String> itemSkus);
  void retryIndexingFailedItems(MandatoryRequestParam mandatoryRequestParam);
}
