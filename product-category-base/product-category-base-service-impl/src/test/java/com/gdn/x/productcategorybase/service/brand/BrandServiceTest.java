package com.gdn.x.productcategorybase.service.brand;


import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;

import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.util.NamedList;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.SolrFieldNames;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.dto.BrandSummaryFilterDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.response.BrandSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandWipRepository;
import com.gdn.x.productcategorybase.repository.sequence.SequenceRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.service.impl.BrandChangeServiceBean;
import com.gdn.x.productcategorybase.service.impl.CacheServiceHelperBean;

public class BrandServiceTest {

  private static final String DEFAULT_BRAND_CODE = "BRD-00000";
  private static final String DEFAULT_BRAND_DELETE_REASON = "brandDeleteReason";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String BRAND_NAME_1 = "3:15 PM";
  private static final String BRAND_NAME_QUERY = "brand_value:3\\:15\\ PM*";
  private static final String DEFAULT_BP_CODE = "BP-CODE";
  private static final String DEFAULT_BRAND_NAME_TEMP = "Blibli.com%";
  private static final String ID = "id";
  private static final String BRAND_ID = "brandId";
  private static final String BRAND_ID_2 = "brandId2";
  private static final String BRAND_WIP_ID = "brandWipId";
  private static final String BRAND_CODE = "brand_code";
  private static final String BRAND_NAME = "brand_value";
  private static final String BRAND_ATTRIBUTE_NAME = "Brand";
  private static final String DEFAULT_BRAND_SEQUENCE = "brand_sequence";
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 1);
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);
  private static final int SEQUENCE = 1;
  private static final long COUNT = 10;
  private static final String DEFAULT_BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String DEFAULT_BRAND_LOGO = "brand_logo";
  private static final String DEFAULT_BRAND_WIP_ID = "brand_wip_id";
  private static final String DEFAULT_PROFILE_BANNER = "profile_banner";
  private static final String DEFAULT_BRAND_DESCRIPTION_OLD = "oldBrandDescription";
  private static final String DEFAULT_BRAND_DESCRIPTION_NEW = "newBrandDescription";
  private static final String HISTORY_CHANGES = "{\"brandDescription\":\"o: oldBrandDescription, n: "
      + "newBrandDescription\"}";
  private static final String DEFAULT_USERNAME = "username";
  private static final String DEFAULT_PRODUCT_CODE = "productCode";

  @Mock
  private BrandRepository brandRepository;

  @Mock
  private SequenceRepository sequenceRepository;

  @Mock
  private AttributeRepository attributeRepository;

  @Mock
  private PredefinedAllowedAttributeValueRepository predefinedAllowedAttributeValueRepository;

  @Mock
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  @Qualifier("brandCollectionClient")
  private CloudSolrClient solrClient;

  @Mock
  private BrandChangeServiceBean brandChangeServiceBean;

  @Mock
  private BrandWipRepository brandWipRepository;

  @Mock
  private BrandWipHistoryService brandWipHistoryService;

  @Mock
  private BrandWipService brandWipService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private AttributeService attributeService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private CacheServiceHelperBean cacheServiceHelperBean;

  @Mock
  private BrandAuthorisationServiceBean brandAuthorisationServiceBean;

  @InjectMocks
  private BrandServiceBean brandServiceBean;

  @Captor
  private ArgumentCaptor<SolrQuery> argumentCaptor;

  @Captor
  private ArgumentCaptor<SolrAddBrandListDomainEventModel> solrAddBrandDomainEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<BrandWip> brandWipArgumentCaptor;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  private BrandWip brandWip;
  private BrandWipResponse brandWipResponse;
  private Brand brand;
  private Brand savedBrand;
  private List<Object[]> brandWips ;
  private String filteredBrandNameForCache = BRAND_NAME.replace(Constants.SPACE, Constants.HYPHEN).toLowerCase();

  @BeforeEach
  public void initializeTest() throws Exception {
    MDC.put("storeId", DEFAULT_STORE_ID);
    MDC.put("username", DEFAULT_USERNAME);
    MockitoAnnotations.initMocks(this);
    brand = this.generateBrand();
    savedBrand = this.generateSavedBrand();
    savedBrand.setProtectedBrand(true);
    Page<Brand> brands = this.generateBrands();
    Attribute attribute = this.generateAttribute();
    Page<Attribute> attributes = this.generateAttributes();
    brandWip = new BrandWip();
    brandWip.setBrandCode(DEFAULT_BRAND_CODE);
    brandWip.setBrandRequestCode(DEFAULT_BRAND_CODE);
    brandWip.setState(BrandWipState.APPROVED);
    brandWipResponse = new BrandWipResponse();
    Mockito.when(
        this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.any(),
            Mockito.any(), Mockito.anyBoolean())).thenReturn(brand);
    Mockito.when(this.sequenceRepository.findByCode(Mockito.anyString())).thenReturn(0L);
    Mockito.when(this.brandRepository.save(any(Brand.class))).thenReturn(brand);
    Mockito.when(
        this.brandRepository.findByStoreIdAndBrandNameAndMarkForDelete(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyBoolean(), any(Pageable.class))).thenReturn(brands);
    Mockito.when(this.attributeRepository.findById(Mockito.anyString())).thenReturn(Optional.of(attribute));
    Mockito.when(
        this.attributeRepository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), any(Pageable.class))).thenReturn(attributes);
    Mockito.when(
        this.predefinedAllowedAttributeValueRepository.save(any(PredefinedAllowedAttributeValue.class)))
        .thenReturn(generatePredefinedAllowedAttributeValue());
    Mockito.when(objectMapper.writeValueAsString(any(HashMap.class))).thenReturn(HISTORY_CHANGES);
    Mockito.doNothing().when(this.predefinedAllowedAttributeValueRepository)
        .deleteByStoreIdAndAttributeIdAndCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.doNothing().when(this.predefinedAllowedAttributeValueRepository)
        .undeleteByStoreIdAndAttributeIdAndCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.brandRepository);
    Mockito.verifyNoMoreInteractions(this.sequenceRepository);
    Mockito.verifyNoMoreInteractions(this.attributeRepository);
    Mockito.verifyNoMoreInteractions(this.predefinedAllowedAttributeValueRepository);
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.domainEventPublisherService);
    Mockito.verifyNoMoreInteractions(this.brandChangeServiceBean, brandWipRepository);
    Mockito.verifyNoMoreInteractions(brandWipService);
    Mockito.verifyNoMoreInteractions(brandWipHistoryService, attributeService);
    Mockito.verifyNoMoreInteractions(predefinedAllowedAttributeValueService);
    Mockito.verifyNoMoreInteractions(this.applicationCacheServiceBean);
    Mockito.verifyNoMoreInteractions(cacheServiceHelperBean);
    MDC.clear();
  }

  private Brand generateSavedBrand() {
    Brand savedBrand = new Brand();
    savedBrand.setId(ID);
    savedBrand.setBrandCode(BrandServiceTest.DEFAULT_BRAND_CODE);
    savedBrand.setBrandDescription(BrandServiceTest.DEFAULT_BRAND_DESCRIPTION_OLD.getBytes());
    savedBrand.setBrandName(BrandServiceTest.DEFAULT_BRAND_NAME);
    savedBrand.setBrandWipId(BRAND_WIP_ID);
    return savedBrand;
  }


  private Brand generateBrand() throws Exception {
    Brand brand = new Brand();
    brand.setId(ID);
    brand.setBrandCode(BrandServiceTest.DEFAULT_BRAND_CODE);
    brand.setBrandName(BrandServiceTest.DEFAULT_BRAND_NAME);
    brand.setBrandDescription(BrandServiceTest.DEFAULT_BRAND_DESCRIPTION_NEW.getBytes());
    return brand;
  }


  private Page<Brand> generateBrands() throws Exception {
    List<Brand> brands = new ArrayList<Brand>();
    brands.add(this.generateBrand());
    return new PageImpl<Brand>(brands);
  }

  private Attribute generateAttribute() throws Exception {
    Attribute attribute = new Attribute();
    return attribute;
  }

  private Page<Attribute> generateAttributes() throws Exception {
    List<Attribute> attributes = new ArrayList<Attribute>();
    attributes.add(this.generateAttribute());
    return new PageImpl<Attribute>(attributes);
  }

  private PredefinedAllowedAttributeValue generatePredefinedAllowedAttributeValue() {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(DEFAULT_BRAND_CODE);
    predefinedAllowedAttributeValue.setStoreId(DEFAULT_STORE_ID);
    return predefinedAllowedAttributeValue;
  }

  private PredefinedAllowedAttributeValue generatePredefinedAllowedAttributeValueForBrand() {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(DEFAULT_BRAND_CODE);
    predefinedAllowedAttributeValue.setStoreId(DEFAULT_STORE_ID);
    predefinedAllowedAttributeValue.setId(ID);
    return predefinedAllowedAttributeValue;
  }

  @Test
  public void createTest() throws Exception {
    Mockito.when(
        this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean())).thenReturn(null);
    Mockito.doNothing().when(this.brandChangeServiceBean).createSolrDocumentForBrandCollection(generateBrand());
    this.brandServiceBean.create(brand);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean());
    Mockito.verify(this.sequenceRepository).findByCode(Mockito.anyString());
    Mockito.verify(this.brandRepository).save(brand);
    Mockito.verify(this.attributeRepository).findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString(), any(Pageable.class));
    Mockito.verify(this.attributeRepository).findById(Mockito.any());
    Mockito.verify(this.predefinedAllowedAttributeValueRepository).save(
        generatePredefinedAllowedAttributeValue());
    Mockito.verify(domainEventPublisherService, Mockito.times(1)).publishBrandUpdated(eq(brand));
    Mockito.verify(this.brandChangeServiceBean).createSolrDocumentForBrandCollection(any());
  }

  @Test
  public void createWithoutCreatePredefinedAllowedAttributeValueBrandTest() throws Exception {
    Page<Attribute> attributes = new PageImpl<Attribute>(new ArrayList<Attribute>());
    Mockito.when(this.brandRepository
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyBoolean())).thenReturn(null);
    Mockito.when(this.attributeRepository
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            any(Pageable.class))).thenReturn(attributes);
    this.brandServiceBean.create(brand);
    Mockito.verify(this.brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyBoolean());
    Mockito.verify(this.sequenceRepository).findByCode(Mockito.anyString());
    Mockito.verify(this.brandRepository).save(any(Brand.class));
    Mockito.verify(this.attributeRepository)
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            any(Pageable.class));
    Mockito.verify(this.attributeRepository, BrandServiceTest.NEVER_CALLED).findById(Mockito.anyString());
    Mockito.verify(this.predefinedAllowedAttributeValueRepository, BrandServiceTest.NEVER_CALLED)
        .save(any(PredefinedAllowedAttributeValue.class));
    Mockito.verify(domainEventPublisherService, Mockito.times(1)).publishBrandUpdated(eq(brand));
    generatePredefinedAllowedAttributeValue();
    Mockito.verify(domainEventPublisherService, Mockito.times(1)).publishBrandUpdated(eq(brand));
    Mockito.verify(this.brandChangeServiceBean).createSolrDocumentForBrandCollection(any());
  }

  @Test
  public void createWithExistExceptionTest() throws Exception {
    Brand brand = new Brand();
    try {
      this.brandServiceBean.create(brand);
    } catch (Exception e) {
      Mockito.verify(this.brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.any(),
          Mockito.any(), Mockito.anyBoolean());
      Mockito.verify(this.sequenceRepository, BrandServiceTest.NEVER_CALLED).findByCode(Mockito.anyString());
      Mockito.verify(this.brandRepository, BrandServiceTest.NEVER_CALLED).save(any(Brand.class));
      Mockito.verify(this.attributeRepository, BrandServiceTest.NEVER_CALLED)
          .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
              any(Pageable.class));
      Mockito.verify(this.attributeRepository, BrandServiceTest.NEVER_CALLED).findById(Mockito.anyString());
      Mockito.verify(this.predefinedAllowedAttributeValueRepository, BrandServiceTest.NEVER_CALLED).save(
          any(PredefinedAllowedAttributeValue.class));
    }
  }

  @Test
  public void findByBrandCodeTest() throws Exception {
    this.brandServiceBean.findByBrandCode(BrandServiceTest.DEFAULT_BRAND_CODE);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.any(),
        Mockito.anyString());
  }

  @Test
  public void findByBrandCodeCacheedTest() throws Exception {
    this.brandServiceBean.findByBrandCodeCached(BrandServiceTest.DEFAULT_STORE_ID, BrandServiceTest.DEFAULT_BRAND_CODE);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void findByBrandCodeCachedExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandServiceBean.findByBrandCodeCached(null, BrandServiceTest.DEFAULT_BRAND_CODE));
  }

  @Test
  public void findByBrandCodeCachedBrandCodeNullExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandServiceBean.findByBrandCodeCached(BrandServiceTest.DEFAULT_BRAND_CODE, null));
  }

  @Test
  public void findSummaryByFilterWithoutFilterTest() throws Exception {
    BrandSummaryFilterDTO filter =
        BrandSummaryFilterDTO.builder().pageable(DEFAULT_PAGEABLE).brandName(BRAND_NAME).markForDelete(true).build();
    Page<Brand> result = brandServiceBean.findSummaryByFilter(filter, DEFAULT_STORE_ID);
    Assertions.assertEquals(1, result.getNumberOfElements());
    Mockito.verify(brandRepository)
        .findByStoreIdAndBrandNameAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME, true, filter.getPageable());
  }

  @Test
  public void findSummaryByFilterSolrServerExceptionTest() throws Exception {
    BrandSummaryFilterDTO filter =
        BrandSummaryFilterDTO.builder().pageable(DEFAULT_PAGEABLE).brandName(BRAND_NAME).markForDelete(false).build();

    Mockito.when(solrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);

    Page<Brand> result = brandServiceBean.findSummaryByFilter(filter, DEFAULT_STORE_ID);

    Assertions.assertEquals(0, result.getNumberOfElements());
    Assertions.assertEquals(new ArrayList<>(), result.getContent());

    Mockito.verify(solrClient).query(argumentCaptor.capture());
  }

  @Test
  public void findSummaryByFilterIOExceptionTest() throws Exception {
    BrandSummaryFilterDTO filter =
        BrandSummaryFilterDTO.builder().pageable(PageRequest.of(0, 10)).build();

    Mockito.when(solrClient.query(any(SolrQuery.class))).thenThrow(IOException.class);

    Page<Brand> result = brandServiceBean.findSummaryByFilter(filter, DEFAULT_STORE_ID);

    Assertions.assertEquals(0, result.getNumberOfElements());
    Assertions.assertEquals(new ArrayList<>(), result.getContent());

    Mockito.verify(solrClient).query(argumentCaptor.capture());
  }

  @Test
  public void findSummaryByFilterWithFilterDescendingTest() throws Exception {
    Date updatedDate = new Date();

    BrandSummaryFilterDTO filter =
        BrandSummaryFilterDTO.builder()
            .pageable(PageRequest.of(0, 10))
            .brandName(BRAND_NAME)
            .updatedDate(updatedDate)
            .sortDirection(SolrQuery.ORDER.desc.name())
            .sortedBy("updatedDate")
            .build();
    NamedList<Object> namedList = new NamedList<>();
    SolrDocumentList solrDocuments = new SolrDocumentList();
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(ID, BRAND_ID);
    SolrDocument solrDocument2 = new SolrDocument();
    solrDocument2.addField(ID, BRAND_ID_2);
    solrDocuments.add(solrDocument);
    solrDocuments.add(solrDocument2);
    solrDocuments.setNumFound(2);
    namedList.add("response", solrDocuments);
    QueryResponse queryResponse = new QueryResponse();
    queryResponse.setResponse(namedList);

    Calendar calendar = Calendar.getInstance();
    Brand brand = new Brand();
    brand.setUpdatedDate(calendar.getTime());
    calendar.add(Calendar.MINUTE, 10);
    Brand brand2 = new Brand();
    brand2.setUpdatedDate(calendar.getTime());
    List<Brand> brandList = Arrays.asList(brand, brand2);

    List<String> brandIds = Arrays.asList(BRAND_ID, BRAND_ID_2);

    Mockito.when(solrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    Mockito.when(brandRepository.findByBrandWipIdIn(brandIds)).thenReturn(brandList);

    Page<Brand> result = brandServiceBean.findSummaryByFilter(filter, DEFAULT_STORE_ID);

    Assertions.assertEquals(2, result.getNumberOfElements());
    Assertions.assertEquals(brand2, result.getContent().get(0));
    Assertions.assertEquals(brand, result.getContent().get(1));

    Mockito.verify(solrClient).query(argumentCaptor.capture());
    Mockito.verify(brandRepository).findByBrandWipIdIn(brandIds);

    SolrQuery solrQuery = argumentCaptor.getValue();
    Assertions.assertEquals("brand_approved:true AND brand_value:brand_value*", solrQuery.getQuery());
    Assertions.assertEquals(1, solrQuery.getSorts().size());
    Assertions.assertEquals(SolrQuery.ORDER.desc, solrQuery.getSorts().get(0).getOrder());
    Assertions.assertEquals(SolrFieldNames.UPDATED_DATE, solrQuery.getSorts().get(0).getItem());
    Assertions.assertEquals(1, solrQuery.getFilterQueries().length);
    Assertions.assertEquals(String.format("updated_date:[%s TO *]",
        DateTimeFormatter.ISO_INSTANT.format(updatedDate.toInstant())),
        solrQuery.getFilterQueries()[0]);
    Assertions.assertEquals(10, solrQuery.getRows().intValue());
    Assertions.assertEquals(0, solrQuery.getStart().intValue());
  }

  @Test
  public void findSummaryByFilterWithFilterAscendingTest() throws Exception {
    Date updatedDate = new Date();

    BrandSummaryFilterDTO filter =
        BrandSummaryFilterDTO.builder()
            .pageable(PageRequest.of(0, 10))
            .brandName(BRAND_NAME)
            .updatedDate(updatedDate)
            .sortDirection(SolrQuery.ORDER.asc.name())
            .sortedBy("updatedDate")
            .build();
    NamedList<Object> namedList = new NamedList<>();
    SolrDocumentList solrDocuments = new SolrDocumentList();
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(ID, BRAND_ID);
    SolrDocument solrDocument2 = new SolrDocument();
    solrDocument2.addField(ID, BRAND_ID_2);
    solrDocuments.add(solrDocument);
    solrDocuments.add(solrDocument2);
    solrDocuments.setNumFound(2);
    namedList.add("response", solrDocuments);
    QueryResponse queryResponse = new QueryResponse();
    queryResponse.setResponse(namedList);

    Calendar calendar = Calendar.getInstance();
    Brand brand = new Brand();
    brand.setUpdatedDate(calendar.getTime());
    calendar.add(Calendar.MINUTE, -10);
    Brand brand2 = new Brand();
    brand2.setUpdatedDate(calendar.getTime());
    List<Brand> brandList = Arrays.asList(brand, brand2);

    List<String> brandIds = Arrays.asList(BRAND_ID, BRAND_ID_2);

    Mockito.when(solrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    Mockito.when(brandRepository.findByBrandWipIdIn(brandIds)).thenReturn(brandList);

    Page<Brand> result = brandServiceBean.findSummaryByFilter(filter, DEFAULT_STORE_ID);

    Assertions.assertEquals(2, result.getNumberOfElements());
    Assertions.assertEquals(brand2, result.getContent().get(0));
    Assertions.assertEquals(brand, result.getContent().get(1));

    Mockito.verify(solrClient).query(argumentCaptor.capture());
    Mockito.verify(brandRepository).findByBrandWipIdIn(brandIds);
  }

  @Test
  public void findSummaryByFilterOrderByNameTest() throws Exception {
    this.brandServiceBean.findSummaryByName(BrandServiceTest.DEFAULT_BRAND_NAME,
        BrandServiceTest.DEFAULT_PAGEABLE);
    Mockito.verify(this.brandRepository)
        .findByStoreIdAndBrandNameAndMarkForDeleteFalseOrderByBrandName(Mockito.anyString(),
            Mockito.anyString(), any(Pageable.class));
  }

  @Test
  public void findBrandNamesByBrandCodesTest() throws Exception {
    Mockito.when(this.brandRepository
        .findByStoreIdAndBrandCodesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Arrays.asList(this.generateBrand()));
    this.brandServiceBean.findBrandNamesByBrandCodes(Arrays.asList(DEFAULT_BRAND_CODE));
    Mockito.verify(this.brandRepository)
        .findByStoreIdAndBrandCodesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void findByBrandNameTest() throws Exception {
    this.brandServiceBean.findByBrandName(BrandServiceTest.DEFAULT_BRAND_NAME, false);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void updateTest() throws Exception {
    brand.setValidBrand(true);
    brand.setProtectedBrand(false);
    brand.setSkuCreationAllowedForAllSellers(false);
    Mockito.when(
      this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(savedBrand);
    Mockito.doNothing().when(brandWipService)
        .updateValidBrandFlag(DEFAULT_USERNAME, brand.getBrandCode(), true);
    this.brandServiceBean.update(brand, DEFAULT_BRAND_REQUEST_CODE, DEFAULT_BRAND_LOGO, DEFAULT_PROFILE_BANNER, DEFAULT_USERNAME,
        true);
    Mockito.verify(this.brandRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.brandRepository).save(any(Brand.class));
    Mockito.verify(this.brandWipHistoryService)
        .generateBrandWipHistory(any(BrandWip.class), eq(HISTORY_CHANGES),
            eq(DEFAULT_USERNAME));
    Mockito.verify(domainEventPublisherService, Mockito.times(1)).publishBrandUpdated(any(Brand.class));
    Mockito.verify(brandWipService).updateValidBrandFlag(DEFAULT_USERNAME, brand.getBrandCode(), true);
    Mockito.verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(brand.getBrandCode());
    Mockito.verify(this.applicationCacheServiceBean).evictProtectedBrandCache(DEFAULT_STORE_ID);
    Mockito.verify(this.brandChangeServiceBean).updateProtectedBrand(Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void updateTestwithSkuCreationAllowedForAllSellersAsNull() throws Exception {
    brand.setValidBrand(true);
    brand.setProtectedBrand(false);
    Mockito.when(
        this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(savedBrand);
    Mockito.doNothing().when(brandWipService)
        .updateValidBrandFlag(DEFAULT_USERNAME, brand.getBrandCode(), true);
    this.brandServiceBean.update(brand, DEFAULT_BRAND_REQUEST_CODE, DEFAULT_BRAND_LOGO, DEFAULT_PROFILE_BANNER, DEFAULT_USERNAME,
        null);
    Mockito.verify(this.brandRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.brandRepository).save(any(Brand.class));
    Mockito.verify(this.brandWipHistoryService)
        .generateBrandWipHistory(any(BrandWip.class), eq(HISTORY_CHANGES),
            eq(DEFAULT_USERNAME));
    Mockito.verify(domainEventPublisherService, Mockito.times(1)).publishBrandUpdated(any(Brand.class));
    Mockito.verify(brandWipService).updateValidBrandFlag(DEFAULT_USERNAME, brand.getBrandCode(), true);
    Mockito.verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(brand.getBrandCode());
    Mockito.verify(this.applicationCacheServiceBean).evictProtectedBrandCache(DEFAULT_STORE_ID);
    Mockito.verify(this.brandChangeServiceBean).updateProtectedBrand(Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void updateSameValidBrandeFlagTest() throws Exception {
    brand.setProtectedBrand(true);
    brand.setSkuCreationAllowedForAllSellers(true);
    Mockito.when(
      this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(savedBrand);
    this.brandServiceBean.update(brand, DEFAULT_BRAND_REQUEST_CODE, DEFAULT_BRAND_LOGO, DEFAULT_PROFILE_BANNER, DEFAULT_USERNAME,
        null);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString());
    Mockito.verify(this.brandRepository).save(any(Brand.class));
    Mockito.verify(this.brandWipHistoryService)
        .generateBrandWipHistory(any(BrandWip.class), eq(HISTORY_CHANGES),
            eq(DEFAULT_USERNAME));
    Mockito.verify(domainEventPublisherService, Mockito.times(1))
        .publishBrandUpdated(any(Brand.class));
  }

  @Test
  public void updateWithNotFoundExceptionTest() throws Exception {
    Mockito.when(
        this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(null);
    Brand brand = new Brand();
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.brandServiceBean.update(brand, DEFAULT_BRAND_REQUEST_CODE, DEFAULT_BRAND_LOGO, DEFAULT_PROFILE_BANNER, DEFAULT_USERNAME,
          null));
    } finally {
      Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.any(),
          Mockito.any());
      Mockito.verify(this.brandRepository, BrandServiceTest.NEVER_CALLED).save(any(Brand.class));
    }
  }

  @Test
  public void deleteTest() throws Exception {
    Mockito.when(
      this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(savedBrand);
    Mockito.when(brandWipService.getBrandWipByStoreIdAndBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(brandWip);
    this.brandServiceBean.delete(DEFAULT_BRAND_CODE, DEFAULT_BRAND_DELETE_REASON);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(this.brandRepository).deleteByStoreIdAndBrandCode(eq(DEFAULT_STORE_ID),Mockito.anyString(),
        eq(DEFAULT_BRAND_CODE));
    Mockito.verify(this.productRepository).countByStoreIdAndBrandIgnoreCase(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    Mockito.verify(this.attributeRepository).findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(
        eq(DEFAULT_STORE_ID), eq(BRAND_ATTRIBUTE_NAME), any(Pageable.class));
    Mockito.verify(this.predefinedAllowedAttributeValueRepository).deleteByStoreIdAndAttributeIdAndCode(
        eq(DEFAULT_STORE_ID), Mockito.any(), eq(DEFAULT_BRAND_CODE));
    Mockito.verify(this.brandWipService).getBrandWipByStoreIdAndBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipService).deleteBrandWip(brandWipArgumentCaptor.capture());
    Mockito.verify(brandWipHistoryService)
        .generateBrandWipHistory(brandWipArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            eq(DEFAULT_USERNAME));
    Mockito.verify(domainEventPublisherService).publishSolrDeleteBrandEvent(
        SolrDeleteBrandDomainEventModel.builder().ids(Collections.singletonList(BRAND_WIP_ID)).build());
    Mockito.verify(domainEventPublisherService)
        .publishBrandDeleted(Mockito.anyString(), eq(DEFAULT_BRAND_CODE));
    Mockito.verify(attributeService)
        .evictAttributeCache(eq(DEFAULT_STORE_ID), Mockito.any(), Mockito.any());
    Mockito.verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(brand.getBrandCode());
    Mockito.verify(this.applicationCacheServiceBean).evictProtectedBrandCache(DEFAULT_STORE_ID);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, brandWipArgumentCaptor.getAllValues().get(0).getBrandCode());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, brandWipArgumentCaptor.getAllValues().get(1).getBrandCode());
    Assertions.assertTrue(stringArgumentCaptor.getValue().contains(DEFAULT_BRAND_DELETE_REASON));
  }


  @Test
  public void deleteWithoutDeletePredefinedAllowedAttributeValueBrandTest() throws Exception {
    Mockito.when(
      this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(savedBrand);
    Mockito.when(brandWipService.getBrandWipByStoreIdAndBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(brandWip);
    Page<Attribute> attributes = new PageImpl<>(new ArrayList<>());
    Mockito.when(this.attributeRepository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), any(Pageable.class))).thenReturn(attributes);
    this.brandServiceBean.delete(DEFAULT_BRAND_CODE, DEFAULT_BRAND_DELETE_REASON);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(this.brandRepository).deleteByStoreIdAndBrandCode(eq(DEFAULT_STORE_ID),Mockito.anyString(),
        eq(DEFAULT_BRAND_CODE));
    Mockito.verify(this.productRepository).countByStoreIdAndBrandIgnoreCase(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    Mockito.verify(this.attributeRepository).findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(
        eq(DEFAULT_STORE_ID), eq(BRAND_ATTRIBUTE_NAME), any(Pageable.class));
    Mockito.verify(this.predefinedAllowedAttributeValueRepository, BrandServiceTest.NEVER_CALLED)
        .deleteByStoreIdAndAttributeIdAndCode(eq(DEFAULT_STORE_ID), Mockito.anyString(),
            eq(DEFAULT_BRAND_CODE));
    Mockito.verify(this.brandWipService).getBrandWipByStoreIdAndBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipService).deleteBrandWip(brandWipArgumentCaptor.capture());
    Mockito.verify(brandWipHistoryService)
        .generateBrandWipHistory(brandWipArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            eq(DEFAULT_USERNAME));
    Mockito.verify(domainEventPublisherService).publishSolrDeleteBrandEvent(
        SolrDeleteBrandDomainEventModel.builder().ids(Collections.singletonList(BRAND_WIP_ID)).build());
    Mockito.verify(domainEventPublisherService)
            .publishBrandDeleted(Mockito.anyString(), eq(DEFAULT_BRAND_CODE));
    Mockito.verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(brand.getBrandCode());
    Mockito.verify(this.applicationCacheServiceBean).evictProtectedBrandCache(DEFAULT_STORE_ID);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, brandWipArgumentCaptor.getAllValues().get(0).getBrandCode());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, brandWipArgumentCaptor.getAllValues().get(1).getBrandCode());
    Assertions.assertTrue(stringArgumentCaptor.getValue().contains(DEFAULT_BRAND_DELETE_REASON));
  }

  @Test
  public void deleteBrandWipNotFoundTest() throws Exception{
    savedBrand.setProtectedBrand(false);
    Page<Attribute> attributes = new PageImpl<>(new ArrayList<>());
    Mockito.when(
      this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(savedBrand);
    Mockito.when(this.attributeRepository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), any(Pageable.class))).thenReturn(attributes);
    this.brandServiceBean.delete(DEFAULT_BRAND_CODE, DEFAULT_BRAND_DELETE_REASON);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(this.brandRepository).deleteByStoreIdAndBrandCode(eq(DEFAULT_STORE_ID),Mockito.anyString(),
        eq(DEFAULT_BRAND_CODE));
    Mockito.verify(this.productRepository).countByStoreIdAndBrandIgnoreCase(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    Mockito.verify(this.attributeRepository).findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(
        eq(DEFAULT_STORE_ID), eq(BRAND_ATTRIBUTE_NAME), any(Pageable.class));
    Mockito.verify(this.predefinedAllowedAttributeValueRepository, BrandServiceTest.NEVER_CALLED).deleteByStoreIdAndAttributeIdAndCode(
        eq(DEFAULT_STORE_ID), Mockito.anyString(), eq(DEFAULT_BRAND_CODE));
    Mockito.verify(this.brandWipService).getBrandWipByStoreIdAndBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(domainEventPublisherService).publishSolrDeleteBrandEvent(
        SolrDeleteBrandDomainEventModel.builder().ids(Collections.singletonList(BRAND_WIP_ID)).build());
    Mockito.verify(domainEventPublisherService)
        .publishBrandDeleted(Mockito.anyString(), eq(DEFAULT_BRAND_CODE));
  }

  @Test
  public void deleteWithNotFoundExceptionTest() throws Exception {
    Mockito.when(
        this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.any(), Mockito.anyString()))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandServiceBean.delete(DEFAULT_BRAND_CODE, DEFAULT_BRAND_DELETE_REASON));
    } finally {
      Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.any(),
          Mockito.anyString());
      Mockito.verify(this.productRepository, BrandServiceTest.NEVER_CALLED).countByStoreIdAndBrandIgnoreCase(
          Mockito.anyString(), Mockito.anyString());
      Mockito.verify(this.brandRepository, BrandServiceTest.NEVER_CALLED).deleteByStoreIdAndBrandCode(
          Mockito.anyString(),Mockito.anyString(), Mockito.anyString());
      Mockito.verify(this.attributeRepository, BrandServiceTest.NEVER_CALLED)
          .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
              any(Pageable.class));
      Mockito.verify(this.predefinedAllowedAttributeValueRepository, BrandServiceTest.NEVER_CALLED)
          .deleteByStoreIdAndAttributeIdAndCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    }
  }

  @Test
  public void deleteWithUsedBrandTest() throws Exception {
    Mockito.when(
      this.brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(savedBrand);
    Mockito
        .when(
            productRepository.countByStoreIdAndBrandIgnoreCase(Mockito.anyString(),
                Mockito.anyString())).thenReturn(1L);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandServiceBean.delete(DEFAULT_BRAND_CODE, DEFAULT_BRAND_DELETE_REASON));
    } finally {
      Mockito.verify(this.brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
          Mockito.anyString(), Mockito.anyString());
      Mockito.verify(this.productRepository).countByStoreIdAndBrandIgnoreCase(Mockito.anyString(),
          Mockito.anyString());
      Mockito.verify(this.brandRepository, BrandServiceTest.NEVER_CALLED).deleteByStoreIdAndBrandCode(
          Mockito.anyString(),Mockito.anyString(), Mockito.anyString());
      Mockito.verify(this.attributeRepository, BrandServiceTest.NEVER_CALLED)
          .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
              any(Pageable.class));
      Mockito.verify(this.predefinedAllowedAttributeValueRepository, BrandServiceTest.NEVER_CALLED)
          .deleteByStoreIdAndAttributeIdAndCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    }
  }

  @Test
  public void undeleteTest() throws Exception {
    Mockito.when(this.brandRepository
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyBoolean())).thenReturn(generateBrand());
    Mockito.doNothing().when(this.brandChangeServiceBean).createSolrDocumentForBrandCollection(generateBrand());
    Brand brand = new Brand();
    brand.setBrandCode(DEFAULT_BRAND_CODE);
    brand.setBrandWipId(DEFAULT_BRAND_WIP_ID);
    brand.setProtectedBrand(true);
    Brand response = this.brandServiceBean.undelete(brand);
    Mockito.verify(this.brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.any(), Mockito.any(),
            Mockito.anyBoolean());
    Mockito.verify(this.brandRepository).save(any(Brand.class));
    Mockito.verify(this.attributeRepository)
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            any(Pageable.class));
    Mockito.verify(this.predefinedAllowedAttributeValueRepository)
        .undeleteByStoreIdAndAttributeIdAndCode(Mockito.any(), Mockito.any(), Mockito.any());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(DEFAULT_BRAND_WIP_ID, response.getBrandWipId());
    Assertions.assertTrue(response.isProtectedBrand());
  }

  @Test
  public void undeleteWithoutUndeletePredefinedAllowedAttributeValueBrandTest() throws Exception {
    Mockito.doNothing().when(this.brandChangeServiceBean).createSolrDocumentForBrandCollection(generateBrand());
    Page<Attribute> attributes = new PageImpl<Attribute>(new ArrayList<Attribute>());
    Mockito.when(
        this.attributeRepository.findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), any(Pageable.class))).thenReturn(attributes);
    Brand brand = new Brand();
    Brand response = this.brandServiceBean.undelete(brand);
    Mockito.verify(this.brandRepository).findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.any(),
        Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.brandRepository).save(any(Brand.class));
    Mockito.verify(this.attributeRepository).findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString(), any(Pageable.class));
    Mockito.verify(this.predefinedAllowedAttributeValueRepository, BrandServiceTest.NEVER_CALLED)
        .undeleteByStoreIdAndAttributeIdAndCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, response.getBrandCode());
  }

  @Test
  public void undeleteWithNotFoundExceptionTest() throws Exception {
    Mockito.when(this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(null);
    Brand brand = new Brand();
    this.brandServiceBean.undelete(brand);
    Mockito.verify(this.brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(Mockito.anyString(), Mockito.any(),
            Mockito.anyBoolean());
    Mockito.verify(this.attributeRepository)
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            any(Pageable.class));
    Mockito.verify(this.predefinedAllowedAttributeValueRepository)
        .undeleteByStoreIdAndAttributeIdAndCode(Mockito.anyString(), Mockito.any(), Mockito.anyString());
    Mockito.verify(brandRepository).save(Mockito.any());
  }

  @Test
  public void getBrandSuggestionsTest() throws Exception {
    SolrQuery solrQuery = new SolrQuery();
    Pageable pageable = PageRequest.of(0, 10);
    QueryResponse queryResponse = new QueryResponse();
    NamedList<Object> namedList = new NamedList<>();
    solrQuery.set("q", "brand_value:*");
    SolrDocumentList solrDocuments = getSolrDocuments();
    namedList.add("response", solrDocuments);
    queryResponse.setResponse(namedList);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(solrClient.query(argumentCaptor.capture())).thenReturn(queryResponse);
    Page<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues =
        this.brandServiceBean.getBrandSuggestions(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, DEFAULT_BP_CODE, pageable, Boolean.FALSE, false);
    Mockito.verify(this.solrClient).query(any(SolrQuery.class));
    Assertions.assertTrue(predefinedAllowedAttributeValues.getContent().size()==1);
  }

  @Test
  public void getBrandSuggestionsNullPBtestTest() throws Exception {
    SolrQuery solrQuery = new SolrQuery();
    Pageable pageable = PageRequest.of(0, 10);
    QueryResponse queryResponse = new QueryResponse();
    NamedList<Object> namedList = new NamedList<>();
    solrQuery.set("q", "brand_value:*");
    SolrDocumentList solrDocuments = new SolrDocumentList();
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(BRAND_NAME, DEFAULT_BRAND_NAME);
    solrDocuments.add(solrDocument);
    namedList.add("response", solrDocuments);
    queryResponse.setResponse(namedList);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(solrClient.query(argumentCaptor.capture())).thenReturn(queryResponse);
    Page<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues =
        this.brandServiceBean.getBrandSuggestions(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, DEFAULT_BP_CODE, pageable, Boolean.FALSE, false);
    Mockito.verify(this.solrClient).query(any(SolrQuery.class));
    Assertions.assertTrue(predefinedAllowedAttributeValues.getContent().size()==1);
    Assertions.assertFalse(predefinedAllowedAttributeValues.getContent().get(0).isProtectedBrand());
  }

  @Test
  public void getBrandSuggestionsWithSpecialCharsTest() throws Exception {
    SolrQuery solrQuery = new SolrQuery();
    Pageable pageable = PageRequest.of(0, 10);
    QueryResponse queryResponse = new QueryResponse();
    NamedList<Object> namedList = new NamedList<>();
    solrQuery.set("q", "brand_value:*");
    SolrDocumentList solrDocuments = getSolrDocuments();
    namedList.add("response", solrDocuments);
    queryResponse.setResponse(namedList);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(solrClient.query(argumentCaptor.capture())).thenReturn(queryResponse);
    Page<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues =
        this.brandServiceBean.getBrandSuggestions(DEFAULT_STORE_ID, BRAND_NAME_1, DEFAULT_BP_CODE, pageable, Boolean.FALSE, false);
    Mockito.verify(this.solrClient).query(argumentCaptor.capture());
    Assertions.assertTrue(predefinedAllowedAttributeValues.getContent().size()==1);
    Assertions.assertEquals(BRAND_NAME_QUERY, argumentCaptor.getValue().getQuery());
  }

  @Test
  public void getBrandSuggestionsTestWithEmptyValue() throws Exception {
    SolrQuery solrQuery = new SolrQuery();
    Pageable pageable = PageRequest.of(0, 10);
    QueryResponse queryResponse = new QueryResponse();
    NamedList<Object> namedList = new NamedList<>();
    solrQuery.set("q", "brand_value:*");
    SolrDocumentList solrDocuments = getSolrDocuments();
    namedList.add("response", solrDocuments);
    queryResponse.setResponse(namedList);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(solrClient.query(argumentCaptor.capture())).thenReturn(queryResponse);
    Page<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues =
        this.brandServiceBean.getBrandSuggestions(DEFAULT_STORE_ID, StringUtils.EMPTY, DEFAULT_BP_CODE, pageable, Boolean.FALSE, false);
    Mockito.verify(this.solrClient).query(any(SolrQuery.class));
    Assertions.assertTrue(predefinedAllowedAttributeValues.getContent().size()==1);
  }

  @Test
  public void getBrandSuggestionsTestWithEmptyValueAndIsExternalTrueAndIsSearchTrue() throws Exception {
    SolrQuery solrQuery = new SolrQuery();
    Pageable pageable = PageRequest.of(0, 10);
    QueryResponse queryResponse = new QueryResponse();
    NamedList<Object> namedList = new NamedList<>();
    solrQuery.set("q", "brand_value:*");
    SolrDocumentList solrDocuments = getSolrDocuments();
    namedList.add("response", solrDocuments);
    queryResponse.setResponse(namedList);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(solrClient.query(argumentCaptor.capture())).thenReturn(queryResponse);
    Page<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues =
        this.brandServiceBean.getBrandSuggestions(DEFAULT_STORE_ID, StringUtils.EMPTY, DEFAULT_BP_CODE, pageable, Boolean.TRUE, true);
    Mockito.verify(this.solrClient).query(any(SolrQuery.class));
    Assertions.assertTrue(predefinedAllowedAttributeValues.getContent().size() == 1);
    Assertions.assertTrue(predefinedAllowedAttributeValues.getContent().get(0).isProtectedBrand());
  }

  private SolrDocumentList getSolrDocuments() {
    SolrDocumentList solrDocuments = new SolrDocumentList();
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(BRAND_NAME, DEFAULT_BRAND_NAME);
    solrDocument.addField(BRAND_CODE, DEFAULT_BRAND_CODE);
    solrDocument.addField(DEFAULT_BRAND_SEQUENCE, SEQUENCE);
    solrDocument.addField(SolrFieldNames.PROTECTED_BRAND, true);
    solrDocuments.add(solrDocument);
    return solrDocuments;
  }

  @Test
  public void getBrandPredefinedValueByCodeAndStateDraftTest() throws Exception {
    brandWipResponse.setState(BrandWipState.DRAFT.name());
    brandWipResponse.setBrandRequestCode(DEFAULT_BRAND_CODE);
    Mockito
        .when(brandWipService.filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(brandWipResponse);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(generatePredefinedAllowedAttributeValueForBrand());
    BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse = this.brandServiceBean
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, Constants.DRAFT_STATUS);
    Mockito.verify(brandWipService)
        .filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertNotNull(brandPredefinedAttributeValueResponse);
    Assertions.assertEquals(ID, brandPredefinedAttributeValueResponse.getId());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, brandPredefinedAttributeValueResponse.getBrandRequestCode());
    Assertions.assertEquals(Constants.DRAFT_STATUS, brandPredefinedAttributeValueResponse.getBrandApprovalStatus());
  }

  @Test
  public void getBrandPredefinedValueByCodeAndStateDraftButApprovedTest() throws Exception {
    brandWipResponse.setState(BrandWipState.APPROVED.name());
    brandWipResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipResponse.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito
        .when(brandWipService.filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandWipResponse);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(generatePredefinedAllowedAttributeValueForBrand());
    BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse = this.brandServiceBean
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE, Constants.DRAFT_STATUS);
    Mockito.verify(brandWipService)
        .filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertNotNull(brandPredefinedAttributeValueResponse);
    Assertions.assertEquals(ID, brandPredefinedAttributeValueResponse.getId());
    Assertions.assertEquals(DEFAULT_BRAND_REQUEST_CODE, brandPredefinedAttributeValueResponse.getBrandRequestCode());
    Assertions.assertEquals(Constants.APPROVED_STATUS, brandPredefinedAttributeValueResponse.getBrandApprovalStatus());
  }

  @Test
  public void getBrandPredefinedValueByCodeAndStateDraftButRejectedTest() throws Exception {
    brandWipResponse.setState(BrandWipState.REJECTED.name());
    brandWipResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipResponse.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito
        .when(brandWipService.filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandWipResponse);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(generatePredefinedAllowedAttributeValueForBrand());
    BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse = this.brandServiceBean
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE, Constants.DRAFT_STATUS);
    Mockito.verify(brandWipService)
        .filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Assertions.assertNull(brandPredefinedAttributeValueResponse);
  }

  @Test
  public void getBrandPredefinedValueByCodeAndStateDraftButDeletedTest() throws Exception {
    brandWipResponse.setState(BrandWipState.DELETED.name());
    brandWipResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    Mockito
        .when(brandWipService.filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandWipResponse);
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(generatePredefinedAllowedAttributeValueForBrand());
    BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse = this.brandServiceBean
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE, Constants.DRAFT_STATUS);
    Mockito.verify(brandWipService)
        .filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Assertions.assertNull(brandPredefinedAttributeValueResponse);
  }

  @Test
  public void getBrandPredefinedValueByCodeAndStateApprovedTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(generatePredefinedAllowedAttributeValueForBrand());
    BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse = this.brandServiceBean
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, Constants.APPROVED_STATUS);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertNotNull(brandPredefinedAttributeValueResponse);
    Assertions.assertEquals(ID, brandPredefinedAttributeValueResponse.getId());
    Assertions.assertEquals(null, brandPredefinedAttributeValueResponse.getBrandRequestCode());
    Assertions.assertEquals(Constants.APPROVED_STATUS, brandPredefinedAttributeValueResponse.getBrandApprovalStatus());
  }

  @Test
  public void getBrandPredefinedValueByCodeAndStateApprovedNullTest() throws Exception {
    Mockito.when(predefinedAllowedAttributeValueService
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(null);
    BrandPredefinedAttributeValueResponse brandPredefinedAttributeValueResponse = this.brandServiceBean
        .getBrandPredefinedValueByCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, Constants.APPROVED_STATUS);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .findByStoreIdAndPredefinedAllowedAttributeCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Assertions.assertNull(brandPredefinedAttributeValueResponse);
  }

  @Test
  public void getDefaultBrandsNullTest() throws Exception {
    Mockito.when(brandRepository.findByStoreIdAndDefaultBrands(DEFAULT_STORE_ID)).thenReturn(null);
    List<PredefinedAllowedAttributeValueResponse> brands = this.brandServiceBean.getDefaultBrands(DEFAULT_STORE_ID);
    Mockito.verify(brandRepository).findByStoreIdAndDefaultBrands(DEFAULT_STORE_ID);
    Assertions.assertTrue(brands.size() == 0);
  }

  @Test
  public void getDefaultBrandsTest() throws Exception {
    Mockito.when(brandRepository.findByStoreIdAndDefaultBrands(DEFAULT_STORE_ID))
        .thenReturn(Collections.singletonList(brand));
    List<PredefinedAllowedAttributeValueResponse> brands = this.brandServiceBean.getDefaultBrands(DEFAULT_STORE_ID);
    Mockito.verify(brandRepository).findByStoreIdAndDefaultBrands(DEFAULT_STORE_ID);
    Assertions.assertNotNull(brands);
  }

  @Test
  public void getNoBrandDefaultBrandsTest() {
    ReflectionTestUtils.setField(this.brandServiceBean, "noBrandSwitch", true);
    Mockito.when(brandRepository.findNoBrandByStoreId(DEFAULT_STORE_ID))
        .thenReturn(Collections.singletonList(brand));
    List<PredefinedAllowedAttributeValueResponse> brands = this.brandServiceBean.getDefaultBrands(DEFAULT_STORE_ID);
    Mockito.verify(brandRepository).findNoBrandByStoreId(DEFAULT_STORE_ID);
    Assertions.assertNotNull(brands);
  }

  @Test
  public void getBrandSummaryResponseForValidBrandsTest() throws Exception {
    Brand brand = generateBrand();
    brand.setBrandWipId(BRAND_WIP_ID);
    brand.setValidBrand(true);
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY))
        .thenReturn(new SystemParameter(Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY, "10",
            Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY));
    Mockito.when(
        brandRepository.findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDescIdAsc(DEFAULT_STORE_ID, PageRequest.of(0, 5)))
        .thenReturn(new PageImpl<>(Arrays.asList(brand), PageRequest.of(0, 5), 10));
    Page<BrandSummaryResponse> brandSummaryResponses = brandServiceBean.getBrandSummaryResponseForValidBrands(DEFAULT_STORE_ID, 0, 5);
    Mockito.verify(systemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY);
    Mockito.verify(brandRepository).findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDescIdAsc(DEFAULT_STORE_ID, PageRequest.of(0, 5));
    Assertions.assertEquals(DEFAULT_BRAND_NAME, brandSummaryResponses.getContent().get(0).getBrandName());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, brandSummaryResponses.getContent().get(0).getBrandCode());
    Assertions.assertEquals(BRAND_WIP_ID, brandSummaryResponses.getContent().get(0).getBrandWipId());
    Assertions.assertTrue(brandSummaryResponses.getContent().get(0).isValidBrand());
  }

  @Test
  public void getBrandSummaryResponseForValidBrandsNullTest() throws Exception {
    Brand brand = generateBrand();
    brand.setBrandWipId(BRAND_WIP_ID);
    brand.setValidBrand(true);
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY))
        .thenReturn(new SystemParameter(Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY, "10",
            Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY));
    Mockito.when(
        brandRepository.findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDescIdAsc(DEFAULT_STORE_ID, PageRequest.of(0, 5)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 5), 10));
    Page<BrandSummaryResponse> brandSummaryResponses = brandServiceBean.getBrandSummaryResponseForValidBrands(DEFAULT_STORE_ID, 0, 5);
    Mockito.verify(systemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY);
    Mockito.verify(brandRepository).findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDescIdAsc(DEFAULT_STORE_ID, PageRequest.of(0, 5));
    Assertions.assertTrue(brandSummaryResponses.getContent().isEmpty());
  }

  @Test
  public void getBrandSummaryResponseForValidBrandsMaxValidationExceptionTest() throws Exception {
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY))
        .thenReturn(new SystemParameter(Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY, "1",
            Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          brandServiceBean.getBrandSummaryResponseForValidBrands(DEFAULT_STORE_ID, 0, 5));
    } finally {
      Mockito.verify(systemParameterService)
          .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY);
    }
  }

  @Test
  public void getBrandSummaryResponseForValidBrandsValidationExceptionTest() throws Exception {
    Mockito.when(
        systemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY))
        .thenReturn(new SystemParameter(Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY, "0",
            Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          brandServiceBean.getBrandSummaryResponseForValidBrands(DEFAULT_STORE_ID, 0, 5));
    } finally {
      Mockito.verify(systemParameterService)
          .findByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.MAX_ALLOWED_SIZE_FOR_BRAND_SUMMARY);
    }
  }

  @Test
  public void getProtectedBrandListTest() {
    Brand brand = new Brand();
    brand.setBrandName(BRAND_NAME);
    brand.setBrandCode(BRAND_CODE);
    List<Brand> brandList = new ArrayList<>();
    brandList.add(brand);
    Mockito.when(
      brandRepository.findByStoreIdAndProtectedBrandTrueAndMarkForDeleteFalse(DEFAULT_STORE_ID))
      .thenReturn(brandList);
    List<ProtectedBrandResponse> protectedBrandList =
      brandServiceBean.getProtectedBrandList(DEFAULT_STORE_ID);
    Mockito.verify(brandRepository)
      .findByStoreIdAndProtectedBrandTrueAndMarkForDeleteFalse(DEFAULT_STORE_ID);
    Assertions.assertEquals(protectedBrandList.get(0).getBrandCode(), BRAND_CODE);
    Assertions.assertEquals(protectedBrandList.get(0).getBrandName(), BRAND_NAME);
  }

  @Test
  public void getEmptyProtectedBrandListTest(){
    List<Brand> brandList = new ArrayList<>();
    Mockito.when(
      brandRepository.findByStoreIdAndProtectedBrandTrueAndMarkForDeleteFalse(DEFAULT_STORE_ID))
      .thenReturn(brandList);
    List<ProtectedBrandResponse> protectedBrandList =
      brandServiceBean.getProtectedBrandList(DEFAULT_STORE_ID);
    Mockito.verify(brandRepository)
      .findByStoreIdAndProtectedBrandTrueAndMarkForDeleteFalse(DEFAULT_STORE_ID);
    Assertions.assertEquals(protectedBrandList.size(), 0);
  }


  @Test
  public void getBrandResponseByBrandNameTest() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setValidBrand(true);
    Mockito.when(cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME,false,
      false, filteredBrandNameForCache)).thenReturn(brandResponse);
    BrandResponse brandResponseByBrandName =
      brandServiceBean.getBrandResponseByBrandName(DEFAULT_STORE_ID, BRAND_NAME, false, false);
    Mockito.verify(cacheServiceHelperBean).findByBrandName(DEFAULT_STORE_ID, BRAND_NAME, false,
      false, filteredBrandNameForCache);
    Assertions.assertEquals(BRAND_NAME, brandResponseByBrandName.getBrandName());
  }

  @Test
  public void getBrandResponseByBrandNameTest_withNullStoreId() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setValidBrand(true);
    Mockito.when(cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME,false,
      false, filteredBrandNameForCache)).thenReturn(brandResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        brandServiceBean.getBrandResponseByBrandName(null, BRAND_NAME, false, false));
  }

  @Test
  public void getBrandResponseByBrandNameTest_withNullBrandName() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setValidBrand(true);
    Mockito.when(cacheServiceHelperBean.findByBrandName(DEFAULT_STORE_ID, BRAND_NAME,false,
      false, filteredBrandNameForCache)).thenReturn(brandResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        brandServiceBean.getBrandResponseByBrandName(DEFAULT_STORE_ID, null, false, false));
  }

  @Test
  public void getBrandResponseByBrandCodeTest() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setValidBrand(true);
    Mockito.when(cacheServiceHelperBean.findByBrandCode(DEFAULT_STORE_ID, BRAND_CODE)).thenReturn(brandResponse);
    BrandResponse brandResponseByBrandCode =
      brandServiceBean.getBrandResponseByBrandCode(DEFAULT_STORE_ID, BRAND_CODE);
    Mockito.verify(cacheServiceHelperBean).findByBrandCode(DEFAULT_STORE_ID, BRAND_CODE);
    Assertions.assertEquals(BRAND_NAME, brandResponseByBrandCode.getBrandName());
  }

  @Test
  public void getBrandResponseByBrandCodeTest_nullStoreID() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setValidBrand(true);
    Mockito.when(cacheServiceHelperBean.findByBrandCode(DEFAULT_STORE_ID, BRAND_CODE)).thenReturn(brandResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        brandServiceBean.getBrandResponseByBrandCode(null, BRAND_CODE));
  }

  @Test
  public void getBrandResponseByBrandCodeTest_nullBrandCode() throws Exception {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setValidBrand(true);
    Mockito.when(cacheServiceHelperBean.findByBrandCode(DEFAULT_STORE_ID, BRAND_CODE)).thenReturn(brandResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        brandServiceBean.getBrandResponseByBrandCode(DEFAULT_STORE_ID, null));
  }

  @Test
  public void deleteAllBrandCacheTest(){
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByCode(DEFAULT_STORE_ID, BRAND_CODE);
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByNameAndActiveBrandsOnly(DEFAULT_STORE_ID, BRAND_NAME);
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByNameAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME);
    brandServiceBean.deleteAllBrandCache(DEFAULT_STORE_ID, BRAND_NAME, BRAND_CODE);
    Mockito.verify(cacheServiceHelperBean).evictBrandCacheByNameAndMarkForDelete(DEFAULT_STORE_ID
      , BRAND_NAME);
    Mockito.verify(cacheServiceHelperBean).evictBrandCacheByCode(DEFAULT_STORE_ID, BRAND_CODE);
    Mockito.verify(cacheServiceHelperBean).evictBrandCacheByNameAndActiveBrandsOnly(DEFAULT_STORE_ID, BRAND_NAME);
    Mockito.verify(cacheServiceHelperBean).evictBrandCacheByNameAndMarkForDelete(DEFAULT_STORE_ID
      , BRAND_NAME);
  }

  @Test
  public void deleteAllBrandCacheWithNullStoreIdTest(){
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByCode(DEFAULT_STORE_ID, BRAND_CODE);
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByNameAndActiveBrandsOnly(DEFAULT_STORE_ID, BRAND_NAME);
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByNameAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> brandServiceBean.deleteAllBrandCache(null, BRAND_NAME, BRAND_CODE));
  }

  @Test
  public void deleteAllBrandCacheWithNullBrandNameTest(){
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByCode(DEFAULT_STORE_ID, BRAND_CODE);
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByNameAndActiveBrandsOnly(DEFAULT_STORE_ID, BRAND_NAME);
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByNameAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> brandServiceBean.deleteAllBrandCache(DEFAULT_STORE_ID, null, BRAND_CODE));
  }

  @Test
  public void deleteAllBrandCacheWithNullBrandCodeTest(){
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByCode(DEFAULT_STORE_ID, BRAND_CODE);
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByNameAndActiveBrandsOnly(DEFAULT_STORE_ID, BRAND_NAME);
    Mockito.doNothing().when(cacheServiceHelperBean).evictBrandCacheByNameAndMarkForDelete(DEFAULT_STORE_ID, BRAND_NAME);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> brandServiceBean.deleteAllBrandCache(DEFAULT_STORE_ID, BRAND_NAME, null));
  }

  @Test
  public void getBrandCodeByProductCodeTest() {
    Product product = new Product();
    product.setProductCode(DEFAULT_PRODUCT_CODE);

    // Create Attribute with the specific brand attribute code
    Attribute attribute = new Attribute();
    attribute.setAttributeCode("BR-M036969");

    Attribute attribute2 = new Attribute();
    attribute2.setAttributeCode("BR-M036970");

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setMarkForDelete(false);

    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setAttribute(attribute2);
    productAttribute2.setMarkForDelete(false);

    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setAttribute(attribute2);
    productAttribute3.setMarkForDelete(true);

    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
        new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(BRAND_CODE);
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    productAttributeValue2.setMarkForDelete(true);
    productAttribute.setProductAttributeValues(List.of(productAttributeValue2, productAttributeValue));
    product.setProductAttributes(List.of(productAttribute2, productAttribute3, productAttribute));

    Mockito.when(
        productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_CODE)).thenReturn(product);

    String result = brandServiceBean.getBrandCodeByProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);

    Mockito.verify(productRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);

    // Assert the result
    Assertions.assertEquals(BRAND_CODE, result);
  }

  @Test
  public void getBrandCodeByProductCodeWithNullProductAttributesTest() {
    Product product = new Product();
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    // Set productAttributes to null to test the first orElse case
    product.setProductAttributes(null);

    Mockito.when(
        productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_CODE)).thenReturn(product);

    String result = brandServiceBean.getBrandCodeByProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);

    Mockito.verify(productRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);

    // Assert the result should be empty string when productAttributes is null
    Assertions.assertEquals(StringUtils.EMPTY, result);
  }

  @Test
  public void getBrandCodeByProductCodeWithNullProductAttributeValuesTest() {
    Product product = new Product();
    product.setProductCode(DEFAULT_PRODUCT_CODE);

    // Create Attribute with the specific brand attribute code
    Attribute attribute = new Attribute();
    attribute.setAttributeCode("BR-M036969");

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setMarkForDelete(false);
    // Set productAttributeValues to null to test the second orElse case
    productAttribute.setProductAttributeValues(null);

    product.setProductAttributes(List.of(productAttribute));

    Mockito.when(
        productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
            DEFAULT_PRODUCT_CODE)).thenReturn(product);

    String result = brandServiceBean.getBrandCodeByProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);

    Mockito.verify(productRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);

    // Assert the result should be empty string when productAttributeValues is null
    Assertions.assertEquals(StringUtils.EMPTY, result);
  }

  @Test
  public void updateOnlyBrandNameTest() throws Exception {
    String newBrandName = "New Brand Name";
    Brand brand = generateBrand();
    brand.setBrandName(DEFAULT_BRAND_NAME);
    brand.setBrandCode(DEFAULT_BRAND_CODE);
    
    BrandWip brandWip = new BrandWip();
    brandWip.setBrandCode(DEFAULT_BRAND_CODE);
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    
    Mockito.when(brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(brand);
    Mockito.when(
            brandWipService.updateBrandName(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, newBrandName))
        .thenReturn(brandWip);
    Mockito.doNothing().when(brandAuthorisationServiceBean)
        .updateBrandNameByBrandCode(DEFAULT_BRAND_NAME, newBrandName, DEFAULT_BRAND_CODE);
    Mockito.when(brandRepository.save(any(Brand.class))).thenReturn(brand);

    brandServiceBean.updateOnlyBrandName(DEFAULT_STORE_ID, newBrandName, DEFAULT_BRAND_CODE);
    
    // Verify all interactions
    Mockito.verify(brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipService).updateBrandName(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, newBrandName);
    Mockito.verify(brandAuthorisationServiceBean).updateBrandNameByBrandCode(DEFAULT_BRAND_NAME, newBrandName, DEFAULT_BRAND_CODE);
    Mockito.verify(brandRepository).save(any(Brand.class));
    Mockito.verify(predefinedAllowedAttributeValueService)
        .updatePredefinedAllowedAttributeCodeForApprovedBrand(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE,
            brand);
    Mockito.verify(cacheServiceHelperBean)
        .evictBrandCacheByCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(cacheServiceHelperBean)
        .evictBrandCacheByNameAndActiveBrandsOnly(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    Mockito.verify(cacheServiceHelperBean)
        .evictBrandCacheByNameAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    String filteredBrandNameForCache  =
        newBrandName.replace(Constants.SPACE, Constants.HYPHEN).toLowerCase();
    Mockito.verify(cacheServiceHelperBean)
        .evictBrandCacheByNameAndActiveBrandsOnly(eq(DEFAULT_STORE_ID), eq(filteredBrandNameForCache));

    // Verify that brand name was updated
    Assertions.assertEquals(newBrandName, brand.getBrandName());
  }

  @Test
  public void updateOnlyBrandNameWithNullBrandTest() throws Exception {
    String newBrandName = "New Brand Name";
    
    Mockito.when(brandRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(null);
    
    // The method should handle null brand gracefully - it will throw NullPointerException
    // when trying to get oldBrandName from null brand
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        brandServiceBean.updateOnlyBrandName(DEFAULT_STORE_ID, newBrandName, DEFAULT_BRAND_CODE));
    
    Mockito.verify(brandRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipService, NEVER_CALLED).getBrandWipByStoreIdAndBrandCode(any(), any());
    Mockito.verify(brandWipService, NEVER_CALLED).updateBrandName(any(), any(), any());
    Mockito.verify(brandAuthorisationServiceBean, NEVER_CALLED).updateBrandNameByBrandCode(any(), any(), any());
    Mockito.verify(brandRepository, NEVER_CALLED).save(any(Brand.class));
    Mockito.verify(domainEventPublisherService, NEVER_CALLED).publishSolrUpdateBrandEvent(any());
    Mockito.verify(domainEventPublisherService, NEVER_CALLED).publishBrandUpdated(any());
  }
}
