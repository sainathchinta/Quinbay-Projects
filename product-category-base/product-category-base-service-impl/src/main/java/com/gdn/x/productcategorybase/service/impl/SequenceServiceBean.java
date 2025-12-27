package com.gdn.x.productcategorybase.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.repository.sequence.SequenceRepository;
import com.gdn.x.productcategorybase.service.SequenceService;

@Service
@Transactional(readOnly = true)
public class SequenceServiceBean implements SequenceService {

  @Autowired
  private SequenceRepository sequenceRepository;

  @Override
  public Long findByCode(String code) {
    return this.sequenceRepository.findByCode(code);
  }
}
