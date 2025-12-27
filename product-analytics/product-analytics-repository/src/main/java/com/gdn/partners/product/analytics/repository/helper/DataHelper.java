package com.gdn.partners.product.analytics.repository.helper;


import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.bson.Document;
import org.springframework.data.mongodb.core.query.Update;

public class DataHelper {

  public static Update getUpdateFromDocument(Document dbObject, String... exclusionList) {
    Set<String> excludedFieldList = new HashSet<>(Arrays.asList(exclusionList));
    Update update = new Update();
    for (Map.Entry<String, Object> fieldAndValues : dbObject.entrySet()) {
      if (!excludedFieldList.contains(fieldAndValues.getKey())) {
        update.set(fieldAndValues.getKey(), fieldAndValues.getValue());
      }
    }
    return update;
  }

}
