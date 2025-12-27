package com.gdn.x.productcategorybase.domainevent;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;

public class SolrDeleteBrandSubscriberTest {

  @Mock
  private SolrBrandRepository solrBrandRepository;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private SolrDeleteBrandSubscriber solrDeleteBrandSubscriber;

  private static final String ID = "ID";
  private static final String MESSAGE = "message";
  private SolrDeleteBrandDomainEventModel solrDeleteBrandDomainEventModel;

  private String message;

  @BeforeEach
  public void init() throws Exception{
    initMocks(this);
    solrDeleteBrandDomainEventModel = SolrDeleteBrandDomainEventModel.builder().ids(Arrays.asList(ID)).build();
    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(solrDeleteBrandDomainEventModel);
    Mockito.when(objectMapper.readValue(message, SolrDeleteBrandDomainEventModel.class))
        .thenReturn(solrDeleteBrandDomainEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(solrBrandRepository);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    when(objectMapper.readValue(MESSAGE, SolrDeleteBrandDomainEventModel.class))
        .thenReturn(solrDeleteBrandDomainEventModel);
    doNothing().when(this.solrBrandRepository).deleteBrandsFromBrandCollectionSolr(Arrays.asList(ID));
    solrDeleteBrandSubscriber.onDomainEventConsumed(MESSAGE);
    verify(solrBrandRepository).deleteBrandsFromBrandCollectionSolr(Arrays.asList(ID));
    verify(objectMapper).readValue(MESSAGE, SolrDeleteBrandDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper).readValue(MESSAGE, SolrDeleteBrandDomainEventModel.class);
    solrDeleteBrandSubscriber.onDomainEventConsumed(MESSAGE);
    verify(objectMapper).readValue(MESSAGE, SolrDeleteBrandDomainEventModel.class);
  }
}