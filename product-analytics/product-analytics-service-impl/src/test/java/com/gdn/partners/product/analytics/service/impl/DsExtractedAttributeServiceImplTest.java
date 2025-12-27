package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.entity.DSExtractionEntity;
import com.gdn.partners.product.analytics.repository.DsExtractionAttributesRepository;
import com.gdn.partners.product.analytics.service.cache.DsExtractedAttributeCacheableService;
import model.AttributeUpdateEventModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.swing.text.html.Option;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import static com.gdn.partners.product.analytics.model.Constants.DS_EXTRACTION;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DsExtractedAttributeServiceImplTest {

    @Mock
    private DsExtractionAttributesRepository dsExtractionAttributesRepository;

    @Mock
    private DsExtractedAttributeCacheableService dsExtractedAttributeCacheableService;

    @InjectMocks
    private DsExtractedAttributeServiceImpl dsExtractedAttributeService;

    private AttributeUpdateEventModel eventModel;
    private DSExtractionEntity existingEntity;

    @BeforeEach
    void setUp() {
        eventModel = new AttributeUpdateEventModel();
        eventModel.setDsAttributeName("testAttribute");
        eventModel.setAttributeCode("testCode");
        eventModel.setUpdatedFields(Collections.singleton(DS_EXTRACTION));

        existingEntity = new DSExtractionEntity();
        existingEntity.setDsAttributeName("testAttribute");
        existingEntity.setAttributeCode("oldCode");
    }

    @Test
    void updateDsExtractedAttribute_WhenEntityExistsAndDsExtractionTrue_ShouldUpdateEntity() {
        // Arrange
        eventModel.setDsExtraction(true);
        when(dsExtractionAttributesRepository.findByDsAttributeName("testAttribute"))
            .thenReturn(existingEntity);

        // Act
        dsExtractedAttributeService.updateDsExtractedAttribute(eventModel);

        // Assert
        verify(dsExtractionAttributesRepository).save(existingEntity);
        verify(dsExtractedAttributeCacheableService)
            .evictCacheByDsAttributeName("testAttribute");
        assert existingEntity.getAttributeCode().equals("testCode");
        assert !existingEntity.isMarkForDelete();
    }

    @Test
    void updateDsExtractedAttribute_WhenEntityExistsAndDsExtractionFalse_ShouldMarkForDelete() {
        // Arrange
        eventModel.setDsExtraction(false);
        when(dsExtractionAttributesRepository.findByDsAttributeName("testAttribute"))
            .thenReturn(existingEntity);

        // Act
        dsExtractedAttributeService.updateDsExtractedAttribute(eventModel);

        // Assert
        verify(dsExtractionAttributesRepository).save(existingEntity);
        verify(dsExtractedAttributeCacheableService)
            .evictCacheByDsAttributeName("testAttribute");
        assert existingEntity.isMarkForDelete();
    }

    @Test
    void updateDsExtractedAttribute_WhenEntityDoesNotExistAndDsExtractionTrue_ShouldCreateNewEntity() {
        // Arrange
        eventModel.setDsExtraction(true);
        when(dsExtractionAttributesRepository.findByDsAttributeName("testAttribute"))
            .thenReturn(null);

        // Act
        dsExtractedAttributeService.updateDsExtractedAttribute(eventModel);

        // Assert
        verify(dsExtractionAttributesRepository).save(any(DSExtractionEntity.class));
        verify(dsExtractedAttributeCacheableService)
            .evictCacheByDsAttributeName("testAttribute");
    }

    @Test
    void updateDsExtractedAttribute_WhenDsExtractionNotInUpdatedFields_ShouldNotUpdate() {
        // Arrange
        eventModel.setUpdatedFields(Set.of("otherField"));

        // Act
        dsExtractedAttributeService.updateDsExtractedAttribute(eventModel);

        // Assert
        verify(dsExtractionAttributesRepository, never()).findByDsAttributeName(any());
        verify(dsExtractionAttributesRepository, never()).save(any());
        verify(dsExtractedAttributeCacheableService, never()).evictCacheByDsAttributeName(any());
    }

    @Test
    void updateDsExtractedAttribute_WhenUpdatedFieldsIsNull_ShouldNotUpdate() {
        // Arrange
        eventModel.setUpdatedFields(null);

        // Act
        dsExtractedAttributeService.updateDsExtractedAttribute(eventModel);

        // Assert
        verify(dsExtractionAttributesRepository, never()).findByDsAttributeName(any());
        verify(dsExtractionAttributesRepository, never()).save(any());
        verify(dsExtractedAttributeCacheableService, never()).evictCacheByDsAttributeName(any());
    }

    @Test
    void updateDsExtractedAttribute_WhenEntityDoesNotExistAndDsExtractionFalse_ShouldReturnEarly() {
        // Arrange
        eventModel.setDsExtraction(false);
        when(dsExtractionAttributesRepository.findByDsAttributeName("testAttribute"))
            .thenReturn(null);

        // Act
        dsExtractedAttributeService.updateDsExtractedAttribute(eventModel);

        // Assert
        verify(dsExtractionAttributesRepository, never()).save(any());
        verify(dsExtractedAttributeCacheableService, never()).evictCacheByDsAttributeName(any());
    }

    @Test
    void getDsExtractedAttributeByAttributeCode_test() {
        when(dsExtractionAttributesRepository.findByAttributeCode("testAttribute")).thenReturn(
            Optional.of(existingEntity));
        DSExtractionEntity dsExtractionEntity =
            dsExtractedAttributeService.getDsExtractedAttributeByAttributeCode("testAttribute");
        verify(dsExtractionAttributesRepository).findByAttributeCode(eq("testAttribute"));
        Assertions.assertEquals(existingEntity.getAttributeCode(),
            dsExtractionEntity.getAttributeCode());
    }

    @Test
    void getDsExtractedAttributeByAttributeCode_testNull() {
        when(dsExtractionAttributesRepository.findByAttributeCode("testAttribute")).thenReturn(
            Optional.ofNullable(null));
        DSExtractionEntity dsExtractionEntity =
            dsExtractedAttributeService.getDsExtractedAttributeByAttributeCode("testAttribute");
        verify(dsExtractionAttributesRepository).findByAttributeCode(eq("testAttribute"));
        Assertions.assertNull(dsExtractionEntity);
    }
} 