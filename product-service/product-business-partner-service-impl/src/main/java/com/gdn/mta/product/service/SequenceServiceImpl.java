package com.gdn.mta.product.service;

import com.gdn.mta.product.repository.SequenceRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class SequenceServiceImpl implements SequenceService {

  @Autowired
  private SequenceRepository sequenceRepository;

  @Override
  public Long findCounterByKey(String key) {
    return sequenceRepository.findByCode(key);
  }
}
