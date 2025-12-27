package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.bulk.entity.Configuration;
import com.gdn.mta.bulk.repository.ConfigurationRepository;

public class ConfigurationServiceBeanTest {

  private static final String DEFAULT_SERVICE_KEY = "SERVICE KEY";
  private static final String DEFAULT_FOLDER = "FOLDER";
  private static final int DEFAULT_AGE_OF_DELETION = 0;
  private static final int DEFAULT_PAGE = 0;
  private static final int DEFAULT_SIZE = 10;
  private Pageable DEFAULT_PAGEABLE;
  private Configuration configuration = new Configuration();
  private List<Configuration> configurationList = new ArrayList<>();
  private Page<Configuration> configurationPage;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    DEFAULT_PAGEABLE =  PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    configuration.setAgeOfDeletion(DEFAULT_AGE_OF_DELETION);
    configuration.setFolderName(DEFAULT_FOLDER);
    configuration.setDisabled(Boolean.FALSE);
    configuration.setServiceKey(DEFAULT_SERVICE_KEY);
    configurationList.add(configuration);
    configurationPage = new PageImpl<>(configurationList);
  }


  @Mock
  private ConfigurationRepository configurationRepository;

  @InjectMocks
  private ConfigurationServiceBean configurationServiceBean;

  @Test
  public void findByServiceKeyTest() {
    Mockito.when(this.configurationRepository.findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE))
        .thenReturn(configurationPage);
    Page<Configuration> response =
        this.configurationServiceBean.findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    Mockito.verify(this.configurationRepository).findByServiceKey(DEFAULT_SERVICE_KEY, DEFAULT_PAGEABLE);
    Assertions.assertEquals(DEFAULT_AGE_OF_DELETION, response.getContent().get(0).getAgeOfDeletion());
    Assertions.assertEquals(DEFAULT_FOLDER, response.getContent().get(0).getFolderName());
    Assertions.assertEquals(DEFAULT_SERVICE_KEY, response.getContent().get(0).getServiceKey());
    Assertions.assertFalse(response.getContent().get(0).isDisabled());
  }
}
