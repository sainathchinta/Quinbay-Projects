package com.gdn.x.productcategorybase.service.impl;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.entity.Lookup;
import com.gdn.x.productcategorybase.repository.LookupRepository;
import com.gdn.x.productcategorybase.service.LookupService;

@Service
@Transactional(readOnly = true)
public class LookupServiceImpl implements LookupService {

  @Autowired
  private LookupRepository lookupRepository;

  @Override
  @Cacheable(value = CacheNames.LOOKUP_CACHE, key = "#lookupGroup", unless = "#result == null")
  public List<Lookup> getLookupByLookupGroup(String lookupGroup) {
    GdnPreconditions
        .checkArgument(StringUtils.isNotBlank(lookupGroup), ErrorMessage.LOOKUP_GROUP_MUST_NOT_BE_BLANK.getMessage());
    return this.lookupRepository.findByLookupGroupAndMarkForDeleteFalse(lookupGroup);
  }
}
