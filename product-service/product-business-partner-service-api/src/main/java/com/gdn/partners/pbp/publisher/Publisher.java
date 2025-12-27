package com.gdn.partners.pbp.publisher;

import java.util.Map;

public interface Publisher {
  
  String SUFFIX_BEAN_NAME = "Publisher";
  
  void publish(Map<String, Object> datas) throws Exception;

}
