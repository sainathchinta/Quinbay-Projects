package com.gdn.partners.pbp.workflow;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface WorkflowProcessor {

  String SUFFIX_BEAN_NAME = "WorkflowProcessor";
  Map<String, WorkflowConfiguration> CONFIGURATIONS = new HashMap<>();
  Map<String, String> WORKERS = new HashMap<>();

  void validate(List<String> states, Map<String, Object> variables) throws Exception;

  void complete(List<String> states, Map<String, Object> variables) throws Exception;

  void generate(List<String> states, Map<String, Object> variables) throws Exception;

  void process(String processCode, Map<String, Object> datas) throws Exception;

  /**
   * delete entries from product workflow history repo by storeId and productCode
   * @param storeId
   * @param productCode
   */
  void deleteProductWfHistoryByStoreIdAndProductCode(String storeId, String productCode);

}
