package com.gdn.mta.bulk.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

class CacheEvictServiceImplTest {

  @InjectMocks
  private CacheEvictServiceImpl cacheEvictService;

  private static final String BP_CODE = "bpCode";

  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  void evictBusinessPartnerCache() {
    cacheEvictService.evictBusinessPartnerCache(BP_CODE);
  }
}