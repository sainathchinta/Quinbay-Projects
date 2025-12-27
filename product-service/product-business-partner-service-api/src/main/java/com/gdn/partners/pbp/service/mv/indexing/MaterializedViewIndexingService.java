package com.gdn.partners.pbp.service.mv.indexing;

import com.gdn.common.web.param.MandatoryRequestParam;

public interface MaterializedViewIndexingService {
  void fullIndexing(MandatoryRequestParam mandatoryRequestParam);

  void partialIndexingByLastIndexedDate(MandatoryRequestParam mandatoryRequestParam);
}
