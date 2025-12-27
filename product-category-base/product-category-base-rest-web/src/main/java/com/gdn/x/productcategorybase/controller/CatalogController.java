package com.gdn.x.productcategorybase.controller;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;


import com.gdn.x.productcategorybase.Constants;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.supercsv.io.ICsvBeanReader;
import org.supercsv.io.ICsvBeanWriter;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.csv.CatalogCsv;
import com.gdn.x.productcategorybase.dto.CustomCategoryDto;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.response.CatalogDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.service.CatalogService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.util.ICommonFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@RestController
@RequestMapping(value = CatalogController.BASE_PATH)
@Tag(name = "CatalogController", description = "Master Catalog Service API")
public class CatalogController {

  @SuppressWarnings("unused")
  private static final Logger LOG = LoggerFactory.getLogger(CatalogController.class);

  private static final String CREATE = "/create";
  private static final String DETAIL = "/{id}";
  private static final String FILTER_TYPE = "/filter/type";
  private static final String FILTER_NAME = "/filter/name";
  private static final String UPDATE = "/update";
  private static final String DELETE = "/delete";
  private static final String UPLOAD = "/upload";
  private static final String CATEGORIES_BY_CATALOGTYPE = "/filter/categories-by-catalogtype";

  public static final String BASE_PATH = "/api/catalog";

  private static final String CATALOG_EXPORT_CSV_FILENAME = "catalog-export.csv";

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private ICommonFactory commonFactory;

  @PostMapping(value = CatalogController.CREATE, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "save new catalog", description = "save new catalog")
  
  public GdnBaseRestResponse createNewCatalog(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CatalogRequest request) throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy()) || StringUtils.isEmpty(request.getCreatedDate())),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getCatalogType())),
        ErrorMessage.ENTITY_REQUIRED_CATALOG_TYPE_FOR_SAVE_MESSAGE.getMessage());

    Catalog catalog = new Catalog();
    BeanUtils.copyProperties(request, catalog, "categories", "catalogType", Constants.ID);
    catalog.setCatalogType(CatalogType.valueOf(request.getCatalogType()));

    if (StringUtils.isEmpty(catalog.getStoreId())) {
      catalog.setStoreId(storeId);
    }
    this.catalogService.save(catalog);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = CatalogController.DELETE, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "delete catalog", description = "delete catalog")
  
  public GdnBaseRestResponse deleteCatalog(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleRequestHolder request) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.ENTITY_REQUIRED_CATALOG_ID_FOR_DELETE_MESSAGE.getMessage());
    this.catalogService.markForDeleteCatalog(storeId, request.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @GetMapping(value = CatalogController.FILTER_NAME, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Catalog by name", description = "get catalog by store id and name and pageable")
  
  public GdnRestListResponse<CatalogResponse> getCatalogByName(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String name) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
        this.catalogService.findByName(storeId, name, pageable));
  }

  @GetMapping(value = CatalogController.FILTER_TYPE, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Catalog by type", description = "get catalog by store id and type and pageable")
  
  public GdnRestListResponse<CatalogResponse> getCatalogByType(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam CatalogType catalogType) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
        this.catalogService.findByCatalogType(storeId, catalogType, pageable));
  }

  @GetMapping(value = CatalogController.DETAIL, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get detail of Master Warehouse",
  description = "get list of Master Warehouse by id and pageable parameter")
  
  public GdnRestSingleResponse<CatalogDetailResponse> getCatalogDetail(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("id") String catalogId) throws Exception {
    CatalogDetailResponse response = new CatalogDetailResponse();
    Catalog catalog = this.catalogService.findDetailByStoreIdAndId(storeId, catalogId);
    BeanUtils.copyProperties(catalog, response);
    response.setCatalogType(catalog.getCatalogType().toString());
    List<CategoryResponse> categories = new ArrayList<CategoryResponse>();
    for (Category category : catalog.getCategories()) {
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(category, categoryResponse);
      if (category.getParentCategory() != null) {
        categoryResponse.setParentCategoryId(category.getParentCategory().getId());
      }
      categories.add(categoryResponse);
    }
    response.setCategories(categories);
    return new GdnRestSingleResponse<CatalogDetailResponse>("", "", true, response, requestId);
  }

  @GetMapping(produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of Catalog", description = "get list of catalog by store id and pageable")
  
  public GdnRestListResponse<CatalogResponse> getCatalogSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
        this.catalogService.findByStoreId(storeId, pageable));
  }

  private GdnRestListResponse<CatalogResponse> populateListResponse(String storeId, String channelId, String clientId,
      String requestId, Pageable pageable, Page<Catalog> catalogPage) throws Exception {
    GdnRestListResponse<CatalogResponse> wrapper = null;
    List<CatalogResponse> catalogResponses = new ArrayList<CatalogResponse>();
    for (Catalog catalog : catalogPage.getContent()) {
      CatalogResponse response = new CatalogResponse();
      BeanUtils.copyProperties(catalog, response);
      response.setCatalogType(catalog.getCatalogType().toString());
      catalogResponses.add(response);
    }
    wrapper = new GdnRestListResponse<CatalogResponse>(null, null, true, catalogResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), catalogPage.getTotalElements()), requestId);
    return wrapper;
  }

  @PostMapping(value = CatalogController.UPDATE, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update catalog", description = "update catalog")
  
  public GdnBaseRestResponse updateCatalog(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CatalogRequest request) throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || StringUtils.isEmpty(request.getUpdatedDate())),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    Catalog catalog = this.catalogService.findByStoreIdAndId(storeId, request.getId());
    GdnPreconditions.checkArgument(catalog != null,
        ErrorMessage.ENTITY_NOT_FOUND_FOR_UPDATE_MESSAGE.getMessage() + request);
    BeanUtils.copyProperties(request, catalog, "categories", "catalogType", "version", "createdBy", "createdDate");
    catalog.setCatalogType(CatalogType.valueOf(request.getCatalogType() ));

    if (StringUtils.isEmpty(catalog.getStoreId())) {
      catalog.setStoreId(storeId);
    }
    this.catalogService.update(catalog);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = CatalogController.UPLOAD, produces = {"text/csv"}, consumes = {
      MediaType.MULTIPART_FORM_DATA_VALUE})
  
  @Operation(summary = "upload catalog", description = "upload catalog")
  public byte[] uploadCatalog(HttpServletRequest request, HttpServletResponse response,
      @RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestPart("catalogCsvFile") MultipartFile catalogCsvFile)
          throws Exception {
    List<String> catalogIds = new ArrayList<String>();
    ICsvBeanReader beanReader = null;
    try {
      beanReader = commonFactory.createCsvBeanReader(catalogCsvFile.getInputStream(), "UTF-8");
      beanReader.getHeader(true);
      CatalogCsv catalogCsv = null;

      while ((catalogCsv =
          beanReader.read(CatalogCsv.class, CatalogCsv.INPUT_HEADER, CatalogCsv.INPUT_PROCESSORS)) != null) {
        Catalog catalog = new Catalog();
        BeanUtils.copyProperties(catalogCsv, catalog);
        catalogIds.add(this.catalogService.save(catalog));
      }
    } finally {
      if (beanReader != null) {
        beanReader.close();
      }
    }

    ICsvBeanWriter beanWriter = null;
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    PrintWriter writer = new PrintWriter(new BufferedOutputStream(out, 1024));
    try {
      beanWriter = commonFactory.createCsvBeanWriter(writer);
      beanWriter.writeHeader(CatalogCsv.ACTUAL_HEADER);

      for (String catalogId : catalogIds) {
        Catalog catalog = this.catalogService.findByStoreIdAndId(storeId, catalogId);
        CatalogCsv catalogCsv = new CatalogCsv();
        BeanUtils.copyProperties(catalog, catalogCsv);
        catalogCsv.setId(catalogId);
        beanWriter.write(catalogCsv, CatalogCsv.OUTPUT_HEADER, CatalogCsv.OUTPUT_PROCESSORS);
      }
    } finally {
      if (beanWriter != null) {
        beanWriter.close();
      }
    }
    out.close();
    byte[] result = out.toByteArray();
    response.setHeader("Content-Disposition",
        "attachment; filename=\"" + CatalogController.CATALOG_EXPORT_CSV_FILENAME + "\"");
    return result;
  }

  @GetMapping(value = CATEGORIES_BY_CATALOGTYPE, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get categories by catalog-type")
  
  public GdnRestSingleResponse<CatalogDetailResponse> getCategoriesFromCatalogType(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam CatalogType catalogType) {
    boolean isSuccess = false;
    String errorMessage = "";
    CatalogDetailResponse catalogDetailResponse = null;
    try {
      LOG.info(
          "invoking Categories From CatalogType in CatalogController. Request Id: {}. "
              + "CatalogType: {}",
          requestId, catalogType);
      List<CustomCategoryDto> listCategories =
          categoryService.getCategoriesFromCatalogType(storeId, catalogType);
      catalogDetailResponse = new CatalogDetailResponse();
      List<CategoryResponse> categoryResponseList = new ArrayList<>();
      for (CustomCategoryDto customCategoryEntity : listCategories) {
        CategoryResponse categoryResponse = new CategoryResponse();
        BeanUtils.copyProperties(customCategoryEntity, categoryResponse);
        categoryResponseList.add(categoryResponse);
      }
      catalogDetailResponse.setCategories(categoryResponseList);
      catalogDetailResponse.setCatalogType(catalogType.toString());
      isSuccess = true;
    } catch (Exception e) {
      LOG.error(
          "Error occurred while invoking Categories From CatalogType in CatalogController. "
              + "Request Id: {}. CatalogType: {} ",
          requestId, catalogType, e);
      errorMessage = e.getMessage();
    }
    return new GdnRestSingleResponse<>(errorMessage, null, isSuccess, catalogDetailResponse,
        requestId);
  }
}
