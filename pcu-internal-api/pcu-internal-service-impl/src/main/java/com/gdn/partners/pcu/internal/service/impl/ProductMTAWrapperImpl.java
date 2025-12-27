package com.gdn.partners.pcu.internal.service.impl;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.partners.pcu.internal.service.MTAService;
import com.gdn.partners.pcu.internal.service.ProductMTAWrapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductMTAWrapperImpl implements ProductMTAWrapper {

  @Autowired
  private MTAService mtaService;

  @Override
  public Map<String, List<String>> getImageOrContentReviewers(String reviewerType) {
    return mtaService.getImageOrContentReviewers(reviewerType);
  }
}