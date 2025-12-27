package com.gdn.partners.pbp.workflow;

import java.util.Map;

public interface WorkflowWorker {
  
  String SUFFIX_BEAN_NAME = "WorkflowWorker";

  void process(Map<String, Object> datas) throws Exception;

}
