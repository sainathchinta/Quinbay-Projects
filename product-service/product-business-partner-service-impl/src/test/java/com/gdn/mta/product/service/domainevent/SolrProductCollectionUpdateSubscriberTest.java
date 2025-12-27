package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.verify;

import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.service.solr.SolrActiveProductCollectionServiceBean;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;

public class SolrProductCollectionUpdateSubscriberTest {
    private SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent;
    private static final String PRODUCT_CODE = "MTA-0001";
    private static final String CATEGORY_CODE = "category_code";
    private ObjectMapper mapper;

    @InjectMocks
    private SolrProductCollectionUpdateSubscriber solrProductCollectionUpdateSubscriber;

    @Mock
    private SolrActiveProductCollectionServiceBean solrActiveProductCollectionServiceBean;

    @Mock
    private ObjectMapper objectMapper;

    @BeforeEach
    public void initializeTest() throws Exception {
        MockitoAnnotations.initMocks(this);
        solrProductCollectionUpdateEvent = new SolrProductCollectionUpdateEvent(generateSolrProductCollectionDTO());
        mapper = new ObjectMapper();
    }

    private SolrProductCollectionDTO generateSolrProductCollectionDTO() {
        SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
        solrProductCollectionDTO.setId(UUID.randomUUID().toString());
        solrProductCollectionDTO.setProductCode(PRODUCT_CODE);
        solrProductCollectionDTO.setCategoryCode(CATEGORY_CODE);
        return solrProductCollectionDTO;
    }

    @AfterEach
    public void finalizeTest() throws Exception {
        Mockito.verifyNoMoreInteractions(this.solrActiveProductCollectionServiceBean);
        Mockito.verifyNoMoreInteractions(this.objectMapper);
    }

    @Test
    public void listenWithNullObject() throws Exception {
        String message = mapper.writeValueAsString(null);
        Mockito.when(objectMapper.readValue(message, SolrProductCollectionUpdateEvent.class))
            .thenReturn(null);
        this.solrProductCollectionUpdateSubscriber.onDomainEventConsumed(message);
        verify(objectMapper).readValue(message, SolrProductCollectionUpdateEvent.class);    }

    @Test
    public void listenWithMessage() throws Exception {
        solrProductCollectionUpdateEvent.setDocumentId(PRODUCT_CODE);
        String message = mapper.writeValueAsString(this.solrProductCollectionUpdateEvent);
        Mockito.when(objectMapper.readValue(message, SolrProductCollectionUpdateEvent.class))
            .thenReturn(this.solrProductCollectionUpdateEvent);
        this.solrProductCollectionUpdateSubscriber.onDomainEventConsumed(message);
        verify(objectMapper).readValue(message, SolrProductCollectionUpdateEvent.class);
        Mockito.verify(solrActiveProductCollectionServiceBean).deleteSolrProductCollectionByDocumentId(Mockito.anyString());
    }

    @Test
    public void listenWithMessageForDocumentEmptyTest() throws Exception {
        String message = mapper.writeValueAsString(this.solrProductCollectionUpdateEvent);
        Mockito.when(objectMapper.readValue(message, SolrProductCollectionUpdateEvent.class))
            .thenReturn(this.solrProductCollectionUpdateEvent);
        this.solrProductCollectionUpdateSubscriber.onDomainEventConsumed(message);
        verify(objectMapper).readValue(message, SolrProductCollectionUpdateEvent.class);
        verify(this.solrActiveProductCollectionServiceBean).addSolrProductCollectionDocument(Mockito.any());
    }

    @Test
    public void listenWithException() throws Exception {
        ArgumentCaptor<SolrProductCollectionUpdateEvent> captor = ArgumentCaptor.forClass(SolrProductCollectionUpdateEvent.class);
        Mockito.doThrow(new RuntimeException()).when(solrActiveProductCollectionServiceBean)
                .addSolrProductCollectionDocument(captor.capture());
        String message = mapper.writeValueAsString(this.solrProductCollectionUpdateEvent);
        Mockito.when(objectMapper.readValue(message, SolrProductCollectionUpdateEvent.class))
            .thenReturn(this.solrProductCollectionUpdateEvent);
        this.solrProductCollectionUpdateSubscriber.onDomainEventConsumed(message);
        Mockito.verify(solrActiveProductCollectionServiceBean).addSolrProductCollectionDocument(solrProductCollectionUpdateEvent);
        verify(objectMapper).readValue(message, SolrProductCollectionUpdateEvent.class);
    }
}
