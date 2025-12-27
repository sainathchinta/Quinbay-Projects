package com.gdn.partners.pbp.service.mv.indexing;

import java.util.Map;

import com.gdn.common.web.param.MandatoryRequestParam;

public interface IndexingService {

  String SUFFIX_BEAN_NAME = "Indexing";

  void doIndexing(MandatoryRequestParam mandatoryRequestParam, Map<String,Object> parameter, boolean isForce) throws Exception;
  
}
