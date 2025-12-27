package com.gdn.x.productcategorybase.solr.dao.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.request.QueryRequest;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.ModifiableSolrParams;
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

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.solr.model.AttributeModel;
import com.gdn.x.productcategorybase.solr.model.ProductDocument;
import com.gdn.x.productcategorybase.solr.model.ReIndexType;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

public class SolrProductDaoImplTest {
	private static final String ID = "ID";
	private static final String NAME = "NAME";
	private static final String KEY = "NAME_val";
	private static final int solrStart = 0;
	private static final int solrRows = 10;

	@InjectMocks
	private SolrProductDaoImpl solrProductDaoImpl;

	@Mock
	private CloudSolrClient solrClient;

	private List<ProductDocument> documentList;
	private List<ProductDocument> documentList2;
	private List<String> deleteList;
	private QueryRequest request;
	private ReIndexType indexType;
	private ProductItem productItem;
	private ProductItem productItem1;


	@BeforeEach
	public void setUp() throws Exception {
		initMocks(this);
		deleteList = new ArrayList<String>();
		deleteList.add("TEST");
		documentList = new ArrayList<ProductDocument>();
		documentList2 = new ArrayList<ProductDocument>();
		ProductDocument document = new ProductDocument();
		document.setId(ID);
		document.setProductName(NAME);
		document.setCategoryId(KEY);
		document.setProductCode(ID);
		document.setUpcCodes(new HashSet<String>(64));
		document.setUpdatedDate(new Date());
		document.setAttributes(new ArrayList<AttributeModel>());
		document.getAttributes().add(new AttributeModel(NAME, NAME));
		document.getAttributes().add(new AttributeModel(null, null));
		document.getAttributes().add(new AttributeModel("id", NAME));
		ProductDocument document2 = new ProductDocument();
		document2.setId(ID);
		document2.setProductName(NAME);
		document2.setCategoryId(KEY);
		document2.setProductCode(ID);
		document2.setUpcCodes(new HashSet<String>(64));
		document2.setUpdatedDate(new Date());
		documentList.add(document);
		documentList.add(document2);
		documentList2.add(document);
		NamedList<Object> namedList = new NamedList<>();
		ModifiableSolrParams params = new ModifiableSolrParams();
		request=new QueryRequest(params);

		Mockito.when(
				solrClient.commit(Mockito.anyBoolean(), Mockito.anyBoolean()))
				.thenReturn(new UpdateResponse());
		Mockito.when(
				solrClient.add((List<SolrInputDocument>) Mockito.any()))
				.thenReturn(new UpdateResponse());
		Mockito.when(solrClient.commit()).thenReturn(new UpdateResponse());
		Mockito.when(solrClient.deleteById(anyList())).thenReturn(new UpdateResponse());
		Mockito.when(
				solrClient.add((List<SolrInputDocument>) Mockito.any()))
				.thenReturn(new UpdateResponse());
		Mockito.when(solrClient.deleteByQuery(anyString())).thenReturn(
				new UpdateResponse());
		Mockito.when(solrClient.request(request)).thenReturn(namedList);
	}

	@AfterEach
	public void tearDown() throws Exception {
		verifyNoMoreInteractions(solrClient);
	}

	@Test
	void commitTest() throws Exception {
		solrProductDaoImpl.commit();
		verify(this.solrClient, times(1)).commit(Mockito.anyBoolean(),
				Mockito.anyBoolean());
	}

	@Test
	public void deleteDocumentsTest() throws Exception {
		solrProductDaoImpl.deleteDocuments(deleteList);
		verify(this.solrClient, times(1)).deleteById(anyList());
	}

	@Test
	public void deleteDocumentsEmptyListTest() throws Exception {
		solrProductDaoImpl.deleteDocuments(new ArrayList<String>());
	}

	@Test
	public void deleteDocumentsExceptionTest() throws Exception {
		Mockito.doThrow(SolrServerException.class).when(this.solrClient).deleteById(anyList());
		solrProductDaoImpl.deleteDocuments(deleteList);
		verify(this.solrClient, times(1)).deleteById(anyList());
	}

	@Test
	public void deleteAllDocumentsTest() throws Exception {
		solrProductDaoImpl.deleteAllDocuments();
		verify(this.solrClient).deleteByQuery(anyString());
		verify(this.solrClient).commit();
	}

	@Test
	public void deleteAllDocumentsExceptionTest() throws Exception {
		Mockito.doThrow(IOException.class).when(this.solrClient).deleteByQuery(anyString());
		Assertions.assertThrows(ApplicationRuntimeException.class, () -> solrProductDaoImpl.deleteAllDocuments());
		verify(this.solrClient, times(1)).deleteByQuery(anyString());
	}

	@Test
	public void postTest() throws Exception {
		solrProductDaoImpl.post(documentList, indexType.DELTA);
		verify(this.solrClient, times(1)).add((List<SolrInputDocument>) Mockito.any());
	}
	
	@Test
	public void postDocumentNullTest() throws Exception {
		documentList2.get(0).setId(null);
		solrProductDaoImpl.post(documentList2, indexType.DELTA);
	}
	
	@Test
	public void postExceptionTest() throws Exception {
		Mockito.doThrow(SolrServerException.class).when(this.solrClient)
			.add((List<SolrInputDocument>) Mockito.any());
		solrProductDaoImpl.post(documentList, indexType.DELTA);
		verify(this.solrClient, times(1)).add((List<SolrInputDocument>) Mockito.any());
	}

	@Test
	public void fullIndexTest() throws IOException, SolrServerException {
		solrProductDaoImpl.fullIndex();
		verify(this.solrClient).request(Mockito.any(QueryRequest.class));
	}

	@Test
	public void fullIndexIOExceptionTest() throws IOException, SolrServerException {
		Mockito.doThrow(IOException.class).when(this.solrClient)
				.request(request);
		solrProductDaoImpl.fullIndex();
		verify(this.solrClient).request(Mockito.any(QueryRequest.class));
	}

	@Test
	public void fullIndexSolrServerExceptionTest() throws IOException, SolrServerException {
		Mockito.doThrow(SolrServerException.class).when(this.solrClient)
				.request(request);
		solrProductDaoImpl.fullIndex();
		verify(this.solrClient).request(Mockito.any(QueryRequest.class));
	}
	@Test
	public void deltaIndexTest() throws IOException, SolrServerException {
		solrProductDaoImpl.deltaIndex();
		verify(this.solrClient).request(Mockito.any(QueryRequest.class));
	}

	@Test
	public void deltaIndexIOExceptionTest() throws IOException, SolrServerException {
		solrProductDaoImpl.deltaIndex();
		verify(this.solrClient).request(Mockito.any(QueryRequest.class));
	}

	@Test
	public void deltaIndexSolrServerExceptionTest() throws IOException, SolrServerException {
		Mockito.doThrow(SolrServerException.class).when(this.solrClient)
				.request(request);
		solrProductDaoImpl.deltaIndex();
		verify(this.solrClient).request(Mockito.any(QueryRequest.class));
	}

	@Captor
	private ArgumentCaptor<SolrQuery> argumentCaptor;

	@Test
	public void findProductItemsWithCategoryIdAndGeneratedItemNameTest()
			throws IOException, SolrServerException {
		QueryResponse queryResponse = new QueryResponse();
		SolrDocumentList solrDocuments = new SolrDocumentList();
		List<ProductItem> productItemList = getProductItems();
		NamedList<Object> namedList = new NamedList<>();
		SolrDocument solrDocument = getSolrDocument();
		solrDocuments.add(solrDocument);
		namedList.add("response", solrDocuments);
		queryResponse.setResponse(namedList);
		SolrQuery query = new SolrQuery();
		query.set("q", "name: AND category_id:");
		query.setStart(0);
		query.setRows(10);
		Mockito.when(solrClient.query(argumentCaptor.capture())).thenReturn(queryResponse);
		List<ProductItem> productItems = solrProductDaoImpl
				.findProductItemsWithCategoryIdAndGeneratedItemName("", "", solrStart, solrRows);
		Assertions.assertEquals(productItemList, productItems);
		Assertions.assertEquals(query.toString(), argumentCaptor.getValue().toString());
		verify(solrClient).query(Mockito.any(SolrQuery.class));
	}

	@Test
	public void findProductItemsWithCategoryIdAndGeneratedItemNameQueryTest()
			throws IOException, SolrServerException {
		QueryResponse queryResponse = new QueryResponse();
		NamedList<Object> namedList = new NamedList<>();
		SolrQuery query = new SolrQuery();
		query.set("q", "name: AND category_id:");
		query.setStart(0);
		query.setRows(10);
		SolrDocumentList solrDocuments = new SolrDocumentList();
		namedList.add("response", solrDocuments);
		queryResponse.setResponse(namedList);
		Mockito.when(solrClient.query(argumentCaptor.capture())).thenReturn(queryResponse);
		List<ProductItem> productItemList = solrProductDaoImpl
				.findProductItemsWithCategoryIdAndGeneratedItemName("", "", solrStart, solrRows);
		verify(solrClient).query(Mockito.any(SolrQuery.class));
		Assertions.assertEquals(query.toString(), argumentCaptor.getValue().toString());
	}

	@Test
	public void findProductItemsWithCategoryIdAndGeneratedItemNameSolrServerExceptionTest()
			throws IOException, SolrServerException {
		Mockito.when(solrClient.query(Mockito.any(SolrQuery.class)))
				.thenThrow(new SolrServerException("SolrServer Exception"));
		solrProductDaoImpl
				.findProductItemsWithCategoryIdAndGeneratedItemName("", "", solrStart, solrRows);
		verify(solrClient).query(Mockito.any(SolrQuery.class));
	}

	@Test
	public void findProductItemsWithCategoryIdAndGeneratedItemNameIOExceptionTest()
			throws IOException, SolrServerException {
		Mockito.when(solrClient.query(Mockito.any(SolrQuery.class)))
				.thenThrow(new IOException("IO Exception"));
		solrProductDaoImpl
				.findProductItemsWithCategoryIdAndGeneratedItemName("", "", solrStart, solrRows);
		verify(solrClient).query(Mockito.any(SolrQuery.class));
	}

	private List<ProductItem> getProductItems() {
		List<ProductItem> productItemList = new ArrayList<>();
		productItem = new ProductItem();
		productItem1 = new ProductItem();
		productItem.setGeneratedItemName("name1");
		Product product = new Product();
		product.setName("test_name");
		product.setProductCode("test_product_code");
		productItem.setProduct(product);
		productItem.setUpcCode("code_1");
		productItem.setSkuCode("code_1");
		ProductItemImage productItemImage = new ProductItemImage();
		List<ProductItemImage> productItemImages = new ArrayList<>();
		productItemImage.setLocationPath("path_1");
		productItemImage.setMainImages(true);
		productItemImages.add(productItemImage);
		productItem.setProductItemImages(productItemImages);
		productItem.setDangerousGoodsLevel(0);
		productItem.setViewable(true);
		productItem.setActivated(true);
		productItemList.add(productItem);

		productItem1.setGeneratedItemName("name2");
		Product product1 = new Product();
		product1.setName("test_name");
		product1.setProductCode("test_product_code");
		productItem1.setProduct(product);
		productItem1.setUpcCode("code_2");
		productItem1.setSkuCode("code_2");
		ProductItemImage productItemImage1 = new ProductItemImage();
		List<ProductItemImage> productItemImages1 = new ArrayList<>();
		productItemImage1.setLocationPath("path_2");
		productItemImage1.setMainImages(true);
		productItemImages1.add(productItemImage1);
		productItem1.setProductItemImages(productItemImages1);
		productItem1.setDangerousGoodsLevel(0);
		productItem1.setViewable(true);
		productItem1.setActivated(true);
		productItemList.add(productItem1);
		return productItemList;
	}

	private SolrDocument getSolrDocument() {
		SolrDocument solrDocument = new SolrDocument();
		solrDocument.setField("name", "test_name");
		Object[] generatedItemNames = {"name1", "name2"};
		solrDocument.setField("generated_item_names", generatedItemNames);
		solrDocument.setField("product_code", "test_product_code");
		Object[] upcCodes = {"code_1", "code_2"};
		solrDocument.setField("upc_codes", upcCodes);
		Object[] skuCodes = {"code_1", "code_2"};
		solrDocument.setField("sku_codes", skuCodes);
		Object[] locationPaths = {"path_1", "path_2"};
		solrDocument.setField("location_paths", locationPaths);
		solrDocument.setField("dangerous_goods_levels", Arrays.asList(new Object[] {0, 0}));
		return solrDocument;
	}

}
