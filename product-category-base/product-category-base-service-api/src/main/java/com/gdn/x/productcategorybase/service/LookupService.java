package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.entity.Lookup;

public interface LookupService {

  /**
   * get the look up based on lookup group
   *
   * @param lookupGroup
   * @return
   */
  List<Lookup> getLookupByLookupGroup(String lookupGroup);

}
