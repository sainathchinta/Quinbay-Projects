package com.gdn.partners.product.analytics.service.impl.cache;

import static org.mockito.MockitoAnnotations.openMocks;

import com.gdn.partners.product.analytics.entity.DSExtractionEntity;
import com.gdn.partners.product.analytics.repository.DsExtractionAttributesRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.verify;

class DsExtractedAttributeCacheableServiceImplTest {

    @InjectMocks
    private DsExtractedAttributeCacheableServiceImpl dsExtractedAttributeCacheableServiceImpl;

    @Mock
    private DsExtractionAttributesRepository dsExtractionAttributesRepository;

    public static final String DS_ATTRIBUTE = "DS_ATTRIBUTE";
    @BeforeEach
    void setUp() {
        openMocks(this);
    }

    @Test
    void evictCacheByDsAttributeName() {
        dsExtractedAttributeCacheableServiceImpl.evictCacheByDsAttributeName(DS_ATTRIBUTE);
    }

    @Test
    void fetchDSExtractionsByName_Success() {
        // Given
        DSExtractionEntity expectedEntity = new DSExtractionEntity();
        expectedEntity.setDsAttributeName(DS_ATTRIBUTE);
        expectedEntity.setMarkForDelete(false);

        when(dsExtractionAttributesRepository.findByDsAttributeName(DS_ATTRIBUTE))
            .thenReturn(expectedEntity);

        // When
        DSExtractionEntity result = dsExtractedAttributeCacheableServiceImpl.fetchDSExtractionsByName(DS_ATTRIBUTE);

        // Then
        assertEquals(expectedEntity, result);
        verify(dsExtractionAttributesRepository).findByDsAttributeName(DS_ATTRIBUTE);
    }

    @Test
    void fetchDSExtractionsByName_NotFound_ReturnsNull() {
        // Given
        when(dsExtractionAttributesRepository.findByDsAttributeName(DS_ATTRIBUTE))
            .thenReturn(null);

        // When
        DSExtractionEntity result = dsExtractedAttributeCacheableServiceImpl.fetchDSExtractionsByName(DS_ATTRIBUTE);

        // Then
        assertNull(result);
        verify(dsExtractionAttributesRepository).findByDsAttributeName(DS_ATTRIBUTE);
    }
}
