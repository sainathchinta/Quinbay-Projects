package com.gdn.mta.bulk.models;

import java.util.List;
import java.util.Map;

import lombok.Data;

@Data
public class BulkInternalDataSplit {
  List<Map<String, String>> duplicateData;
  List<Map<String, String>> uniqueData;
}
