package com.gdn.x.mta.distributiontask.service.impl;

import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.dao.api.VendorFilterRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorQuotaCounterRepository;

import com.gdn.x.mta.distributiontask.model.VendorDefaultFilter;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.PivotField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.util.NamedList;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.x.mta.distributiontask.dao.api.VendorRepository;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.VendorTaskInformationDTO;
import com.gdn.x.mta.distributiontask.model.enums.VendorProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.impl.util.VendorUtils;

/**
 * Created by Alok on 9/20/16.
 */

@ExtendWith(MockitoExtension.class)
public class VendorServiceImplTest {

  @InjectMocks
  private VendorServiceImpl vendorServiceImpl;

  @Mock
  private VendorRepository vendorRepository;

  @Mock
  private VendorQuotaCounterRepository vendorQuotaCounterRepository;

  @Mock
  private CloudSolrClient cloudSolrClient;

  @Mock
  private VendorUtils vendorUtils;

  @Mock
  private QueryResponse queryResponse;

  @Mock
  private NamedList namedList;

  @Mock
  private VendorFilterRepository vendorFilterRepository;

  private ClassLoader classLoader;
  private ObjectMapper objectMapper = new ObjectMapper();

  public static final String VENDOR_CODE_STATE = "vendorCode,state";
  public static final String VENDOR_ASSIGNEE = "vendorAssignee";
  public static final String STORE_ID = "10001";
  public static final String VENDOR_EMAIL = "vendorEmail";

  private List<Vendor> createVendorList() {
    List<Vendor> vendorList = new ArrayList<>();
    Vendor vendor = new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").build();
    return vendorList;
  }

  @Test public void saveTestWithIdOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").isAbleToReject(false)
            .isQcRequired(false).build();
    Mockito.when(vendorRepository.save(Mockito.any(Vendor.class))).thenReturn(vendor);
    Mockito.when(vendorRepository.findById(Mockito.anyString())).thenReturn(Optional.of(vendor));
    vendorServiceImpl.save(vendor);
    Mockito.verify(vendorRepository).save(Mockito.any(Vendor.class));
  }

  @Test public void saveTestOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode("vendorCode").name("name").isAbleToReject(false)
            .isQcRequired(false).build();
    Mockito.when(vendorRepository.save(Mockito.any(Vendor.class))).thenReturn(vendor);
    vendorServiceImpl.save(vendor);
    Mockito.verify(vendorRepository).save(Mockito.any(Vendor.class));
  }

  @Test
   void getVendorListTest() throws Exception{
    Page<Vendor> vendorList = new PageImpl<Vendor>(createVendorList());
    Mockito.when(vendorRepository.getVendorList(Mockito.any(Pageable.class))).thenReturn(vendorList);
    Pageable pageable = PageRequest.of(0,25);
    vendorServiceImpl.findVendorList(pageable);
  }

  @Test
   void getVendorByCodeTestOk() throws Exception {
    Vendor vendor = new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").build();
    Mockito.when(vendorRepository.findByVendorCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(vendor);
    vendorServiceImpl.findByVendorCode("vendorCode");

  }

  private List<Object[]> createVendorInformationTaskNative(){
    List<Object[]> results = new ArrayList<>();
    Object[] result = new Object[6];
    result[0] = "id";
    result[1] = "vendorCode";
    result[2] = "name";
    result[3] = BigDecimal.valueOf(1L);
    result[4] = BigDecimal.valueOf(2L);
    result[5] = 10;
    results.add(result);
    return results;
  }

  private List<Object[]> createVendorCapacityNative(){

    Object[] result = new Object[6];
    result[0] = "id";
    result[1] = "vendorCode";
    result[2] = "name";
    result[3] = new Date();
    result[4] = new Date();
    result[5] = 10;
    Object[] result2 = new Object[6];
    result[0] = "id";
    result[1] = "vendorCode";
    result[2] = "name";
    result[3] = null;
    result[4] = null;
    result[5] = 10;
    List<Object[]> results = new ArrayList<>(Arrays.asList(result, result2));
    return results;
  }

  @Test
   void countVendorAssignationAndCapacityTest() throws IOException, SolrServerException {
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(Exception.class,
        () -> vendorServiceImpl.countVendorAssignationAndCapacity());
    }finally {
      Mockito.verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    }
  }

  @Test
   void countVendorAssignationAndCapacitySolrExceptionTest() throws IOException, SolrServerException {
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenThrow(SolrException.class);
    try {
      Assertions.assertThrows(Exception.class,
        () -> vendorServiceImpl.countVendorAssignationAndCapacity());
    } finally {
      Mockito.verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    }
  }

  @Test
   void countVendorAssignationAndCapacityIOExceptionTest() throws IOException, SolrServerException {
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenThrow(IOException.class);
    try {
      Assertions.assertThrows(Exception.class,
        () -> vendorServiceImpl.countVendorAssignationAndCapacity());
    } finally {
      Mockito.verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    }
  }

  @Test
   void countVendorAssignationAndCapacityTestSolrTestNullResponse() throws IOException, SolrServerException {
    PivotField pivotField = new PivotField(VendorProductSolrFieldNames.VENDOR_CODE, "qrval", 10000, Arrays.asList(
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.PASSED.name(), 800,
            new ArrayList<>(), new HashMap<>(), new HashMap<>(), new ArrayList<>()),
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.PASSED.name(), 100, new ArrayList<>(),
            new HashMap<>(), new HashMap<>(), new ArrayList<>()),
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.REJECTED.name(), 100, new ArrayList<>(),
            new HashMap<>(), new HashMap<>(), new ArrayList<>())), new HashMap<>(), new HashMap<>(), new ArrayList<>());
    PivotField pivotField1 = new PivotField(VendorProductSolrFieldNames.VENDOR_CODE, "XWyfN", 10000, Arrays.asList(
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.PASSED.name(), 800,
            new ArrayList<>(), new HashMap<>(), new HashMap<>(), new ArrayList<>()),
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.PASSED.name(), 100, new ArrayList<>(),
            new HashMap<>(), new HashMap<>(), new ArrayList<>()),
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.REJECTED.name(), 100, new ArrayList<>(),
            new HashMap<>(), new HashMap<>(), new ArrayList<>())), new HashMap<>(), new HashMap<>(), new ArrayList<>());
    PivotField pivotField2 = new PivotField(VendorProductSolrFieldNames.VENDOR_CODE, "TQN77", 10000, Arrays.asList(
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.PASSED.name(), 800,
            new ArrayList<>(), new HashMap<>(), new HashMap<>(), new ArrayList<>()),
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.IN_REVIEW.name(), 100, new ArrayList<>(),
            new HashMap<>(), new HashMap<>(), new ArrayList<>()),
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.REJECTED.name(), 100, new ArrayList<>(),
            new HashMap<>(), new HashMap<>(), new ArrayList<>())), new HashMap<>(), new HashMap<>(), new ArrayList<>());
    PivotField pivotField3 = new PivotField(VendorProductSolrFieldNames.VENDOR_CODE, "NA", 10000, Arrays.asList(
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.PASSED.name(), 800,
            new ArrayList<>(), new HashMap<>(), new HashMap<>(), new ArrayList<>()),
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.PASSED.name(), 100, new ArrayList<>(),
            new HashMap<>(), new HashMap<>(), new ArrayList<>()),
        new PivotField(VendorProductSolrFieldNames.STATE, WorkflowState.REJECTED.name(), 100, new ArrayList<>(),
            new HashMap<>(), new HashMap<>(), new ArrayList<>())), new HashMap<>(), new HashMap<>(), new ArrayList<>());
    String vendorFilePath = classLoader.getResource("vendor").getPath();
    File vendorFile = new File(vendorFilePath + File.separator + "VendorList.json");
    List<Vendor> vendors = objectMapper.readValue(vendorFile, new TypeReference<List<Vendor>>() {
    });
    Mockito.when(this.vendorRepository.findByMarkForDeleteFalse()).thenReturn(vendors);
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(null);
    List<VendorTaskInformationDTO> vendorTaskInformationDTOS = vendorServiceImpl.countVendorAssignationAndCapacity();
    Mockito.verify(this.cloudSolrClient, Mockito.times(1))
        .query(Mockito.any(SolrQuery.class));
    Mockito.verify(vendorRepository).findByMarkForDeleteFalse();
    Assertions.assertEquals(10, vendorTaskInformationDTOS.size());
    Assertions.assertEquals(0, vendorTaskInformationDTOS.get(9).getAssignedCount(), 0);
  }

  @Test
   void countVendorAssignationAndCapacityTestSolrEmptyPivotsTest() throws IOException, SolrServerException {
    when(this.queryResponse.getFacetPivot()).thenReturn(namedList);
    when(this.namedList.get(VENDOR_CODE_STATE)).thenReturn(new ArrayList<>());
    String vendorFilePath = classLoader.getResource("vendor").getPath();
    File vendorFile = new File(vendorFilePath + File.separator + "VendorList.json");
    List<Vendor> vendors = objectMapper.readValue(vendorFile, new TypeReference<List<Vendor>>() {
    });
    Mockito.when(this.vendorRepository.findByMarkForDeleteFalse()).thenReturn(vendors);
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    List<VendorTaskInformationDTO> vendorTaskInformationDTOS = vendorServiceImpl.countVendorAssignationAndCapacity();
    Mockito.verify(this.vendorRepository, Mockito.times(1))
        .findByMarkForDeleteFalse();
    Mockito.verify(this.cloudSolrClient, Mockito.times(1))
        .query(Mockito.any(SolrQuery.class));
    Mockito.verify(queryResponse).getFacetPivot();
    Mockito.verify(namedList).get(VENDOR_CODE_STATE);
    Assertions.assertEquals(10, vendorTaskInformationDTOS.size());
    Assertions.assertEquals(0, vendorTaskInformationDTOS.get(9).getAssignedCount(), 0);
  }

  @Test
   void countVendorsCapacityTest() {
    Mockito.when(this.vendorRepository.countAllVendorRemainingCapacity())
        .thenReturn(createVendorCapacityNative());
    vendorServiceImpl.countVendorsCapacity();
    Mockito.verify(this.vendorRepository, Mockito.times(1))
        .countAllVendorRemainingCapacity();
    Mockito.verify(this.vendorUtils, Mockito.times(2))
        .convertToVendorCapacityDTO((Object[]) Mockito.any());
  }

  @Test
   void countProductAssignedToVendorTestOk() throws Exception {
    Vendor vendor = new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").build();
    Integer count = null;
    Mockito.when(vendorRepository.assignedProductCount(Mockito.anyString()))
        .thenReturn(count);
    vendorServiceImpl.assignedProductCount("vendorCode");
    Mockito.verify(this.vendorRepository, Mockito.times(1))
        .assignedProductCount(Mockito.anyString());
  }

  @Test public void deleteVendorTestOk() throws Exception {
    Vendor vendor = new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").build();
    vendorServiceImpl.deleteVender(vendor);
    Mockito.verify(this.vendorRepository, Mockito.times(1)).setVendorQuota(Mockito.anyString());
    Mockito.verify(this.vendorRepository, Mockito.times(1)).save(Mockito.any(Vendor.class));
  }

  @Test
   void deleteVendorTestNotOk() throws Exception {
    Vendor vendor =
        new Vendor.Builder().vendorCode(null).name("name").isAbleToReject(false).isQcRequired(false)
            .build();
      vendorServiceImpl.deleteVender(vendor);
      Mockito.verify(this.vendorRepository, Mockito.times(0)).save(Mockito.any(Vendor.class));
  }

  @Test
   void saveDefaultSettingsFilterTest() throws Exception {
    VendorDefaultFilter vendorDefaultFilter = new VendorDefaultFilter();
    VendorDefaultFilterRequest vendorDefaultFilterRequest = new VendorDefaultFilterRequest();
    List<String> assigneeList = new ArrayList<>();
    assigneeList.add(VENDOR_ASSIGNEE);
    vendorDefaultFilterRequest.setAssigneeList(assigneeList);
    vendorDefaultFilter.setAssigneeList(objectMapper.writeValueAsString(vendorDefaultFilterRequest.getAssigneeList()));
    Mockito.when(vendorFilterRepository.findByStoreIdAndVendorEmailAndMarkForDeleteFalse(STORE_ID, null))
        .thenReturn(null);
    vendorServiceImpl.saveDefaultSettingFilter(vendorDefaultFilterRequest, STORE_ID);
    Mockito.verify(vendorFilterRepository).saveAndFlush(Mockito.any(VendorDefaultFilter.class));
    Mockito.verify(vendorFilterRepository).findByStoreIdAndVendorEmailAndMarkForDeleteFalse(STORE_ID, null);
  }

  @Test
   void getDefaultSettingsFilterTest() throws Exception {
    List<String> assigneeList = new ArrayList<>();
    assigneeList.add(VENDOR_ASSIGNEE);
    VendorDefaultFilter vendorDefaultFilter = new VendorDefaultFilter();
    vendorDefaultFilter.setVendorEmail(VENDOR_EMAIL);
    vendorDefaultFilter.setAssigneeList(objectMapper.writeValueAsString(assigneeList));
    Mockito.when(vendorFilterRepository.findByStoreIdAndVendorEmailAndMarkForDeleteFalse(STORE_ID, VENDOR_EMAIL)).thenReturn(vendorDefaultFilter);
    VendorDefaultFilterResponse vendorDefaultFilterResponse =
            vendorServiceImpl.getDefaultSettingFilter(STORE_ID, VENDOR_EMAIL);
    Mockito.verify(vendorFilterRepository).findByStoreIdAndVendorEmailAndMarkForDeleteFalse(STORE_ID, VENDOR_EMAIL);
    Assertions.assertEquals(assigneeList, vendorDefaultFilterResponse.getAssigneeList());
  }

  @BeforeEach
  public void setUp(){
    classLoader = getClass().getClassLoader();  }

  @AfterEach
  public void tearDown(){
    Mockito.verifyNoMoreInteractions(queryResponse);
    Mockito.verifyNoMoreInteractions(cloudSolrClient);
    Mockito.verifyNoMoreInteractions(namedList);
    Mockito.verifyNoMoreInteractions(vendorFilterRepository);
  }

}
