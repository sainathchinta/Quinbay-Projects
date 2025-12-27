package com.gdn.mta.bulk;

public interface BulkProcessValidationErrorMessages {

  String ATTRIBUTE_VALUE_INVALID =
      "Informasi atribut wajib tidak boleh kosong";

  String ATTRIBUTE_VALUE_INVALID_EN =
      "Mandatory attribute information should not be empty";

  String ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK = "Nilai atribut tidak boleh kosong";

  String ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN = "Attribute value cannot be empty";

  String ATTRIBUTE_VALUES_INVALID = "Nilai atribut tidak sesuai";

  String EAN_UPC_VALUES_INVALID = "EAN/UPC tidak sesuai";

  String INVALID_WARNA_VALUE = "Nilai warna tidak boleh kosong";

  String INVALID_WARNA_VALUE_EN = "Warna value cannot be empty";

  String ATTRIBUTE_VALUES_INVALID_EN = "Invalid attribute value";

  String OPTIONAL_ATTRIBUTE_VALUE = "Atribut opsional ini wajib diisi semua atau dibiarkan kosong";

  String OPTIONAL_ATTRIBUTE_VALUE_EN = "This optional attribute must be all filled or left empty";

  String EAN_UPC_VALUES_INVALID_EN = "Invalid EAN/UPC value";

  String CATEGORY_INVALID = "Kategori tidak sesuai";

  String CATEGORY_INVALID_EN = "Inappropriate category";

  String IMAGES_MUST_NOT_BE_BLANK = "Foto tidak ditemukan. Upload min. 1 foto untuk melanjutkan";

  String IMAGES_MUST_NOT_BE_BLANK_EN = "Photo not found. Please upload min. 1 photo to continue";

  String FIRST_IMAGE_MUST_NOT_BE_BLANK = "Gambar pertama tidak boleh kosong";

  String FIRST_IMAGE_MUST_NOT_BE_BLANK_EN = "First image must not be blank";

  String IMAGE_FILE_NOT_FOUND = "file gambar tidak ditemukan";

  String IMAGE_FILE_NOT_FOUND_EN = "Image file not found";

  String IMAGE_FILE_TYPE_INVALID = "Tipe file gambar salah, hanya jpg, png, dan jpeg yang diizinkan";

  String IMAGE_FILE_TYPE_INVALID_EN = "Incorrect image file type, only jpg, png, and jpeg are allowed";

  String IMAGE_URL_TYPE_INVALID = "Tipe file URL salah, hanya jpg, png, dan jpeg yang diizinkan";

  String IMAGE_URL_TYPE_INVALID_EN = "Incorrect image URL, only jpg, png, and jpeg are allowed";

  String IMAGE_URL_INVALID = "Tipe file URL salah";

  String IMAGE_URL_INVALID_EN = "Incorrect image URL";

  String HTTP_IMAGE_URL_INVALID = "Gambar URL salah, hanya https URL yang diizinkan";

  String HTTP_IMAGE_URL_INVALID_EN = "Incorrect image URL, only https URL allowed";

  String PRODUCT_NAME_MUST_NOT_BE_BLANK = "Nama produk tidak boleh kosong";

  String PRODUCT_NAME_MUST_NOT_BE_BLANK_EN = "Product name cannot be empty";

  String PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS = "Nama produk tidak boleh lebih dari 150 karakter";

  String PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS_EN = "Product name can't have more than 150 characters";

  String PRODUCT_TYPE_MUST_NOT_BE_BLANK = "Tipe produk tidak boleh kosong";

  String PRODUCT_TYPE_MUST_NOT_BE_BLANK_EN = "Product type cannot be empty";

  String PRODUCT_TYPE_INVALID = "Tipe Penanganan tidak sesuai. Harus berada dalam kisaran 1-3";

  String PRODUCT_TYPE_INVALID_EN = "Handling Type is not suitable. Must be in the range 1-3";

  String PRICE_MUST_NOT_BE_BLANK = "Harga tidak boleh kosong";

  String PRICE_MUST_NOT_BE_BLANK_EN = "Price cannot be empty";

  String BFB_BASE_PRICE_MUST_NOT_BE_BLANK_EN = "Bfb base price cannot be empty";

  String PRICE_INVALID = "Harga tidak sesuai";

  String PRICE_INVALID_EN = "Price is not appropriate";

  String BFB_BASE_PRICE_INVALID_EN = "Bfb base price is not appropriate";

  String BFB_BASE_PRICE_CANNOT_BE_ZERO_EN = "Bfb base price should be greater than 0";

  String SALE_PRICE_MUST_NOT_BE_BLANK = "Harga penjualan tidak boleh kosong atau nol";

  String SALE_PRICE_MUST_NOT_BE_BLANK_EN = "Sale price cannot be empty";

  String SALE_PRICE_INVALID = "Harga penjualan tidak sesuai";

  String SALE_PRICE_INVALID_EN = "Sale price is not appropriate";

  String MINIMUM_PRICE_VALUE_INVALID = "Harga harus lebih besar dari ";

  String MINIMUM_PRICE_VALUE_INVALID_EN = "Price must be greater than ";

  String MINIMUM_SALE_PRICE_VALUE_INVALID = "Harga penjualan harus lebih besar dari ";

  String MINIMUM_SALE_PRICE_VALUE_INVALID_EN = "Sale price must be greater than ";

  String MAXIMUM_SALE_PRICE_VALUE_INVALID = "Harga penjualan tidak boleh lebih besar dari harga";

  String MAXIMUM_SALE_PRICE_VALUE_INVALID_EN = "The sale price cannot be greater than the price";

  String STOCK_INVALID = "Stok tidak sesuai";

  String STOCK_INVALID_EN = "Stock is not valid";

  String MINIMUM_STOCK_VALUE_INVALID = "Stok tidak boleh kurang dari 0";

  String MINIMUM_STOCK_VALUE_INVALID_EN = "Stock cannot be less than 0";

  String MAXIMUM_STOCK_VALUE_INVALID = "Stok tidak boleh lebih dari %s";

  String MAXIMUM_STOCK_VALUE_INVALID_EN = "Stock cannot be more than %s";

  String MINIMUM_STOCK_MUST_NOT_BE_BLANK = "Stok minimum wajib diisi";

  String MINIMUM_STOCK_MUST_NOT_BE_BLANK_EN = "Minimum stock is required";

  String MINIMUM_STOCK_INVALID = "Stok minimum tidak sesuai";

  String MINIMUM_STOCK_INVALID_EN = "Minimum stock is not appropriate";

  String MINIMUM_MIN_STOCK_VALUE_INVALID = "Stok minimum tidak boleh kurang dari 0";

  String MINIMUM_MIN_STOCK_VALUE_INVALID_EN = "Minimum stock cannot be less than 0";

  String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK = "Kode pickup point tidak boleh kosong";

  String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK_EN = "The pickup point code cannot be empty";
  String BULK_PROCESS_CODE_MUST_NOT_BE_BLANK = "The bulk process code cannot be empty";

  String CNC_STATUS_VALUE_INVALID = "Nilai status CNC tidak valid";

  String CNC_STATUS_VALUE_INVALID_EN = "The CNC status value is invalid";

  String DELIVERY_STATUS_VALUE_INVALID = "Nilai status pengiriman tidak valid";

  String DELIVERY_STATUS_VALUE_INVALID_EN = "The delivery status value is invalid";
  String INSTORE_STATUS_VALUE_INVALID_EN = "The Instore status value is invalid";
  String INSTORE_STATUS_VALUE_INVALID_ID = "Nilai status Instore tidak valid";

  String BFB_BUYABLE_VALUE_INVALID_EN = "The bfb buyable value is invalid";

  String BFB_MANAGED_VALUE_INVALID_EN = "The bfb managed value is invalid";

  String LENGTH_MUST_NOT_BE_BLANK = "Panjang tidak boleh kosong";

  String LENGTH_MUST_NOT_BE_BLANK_EN = "Length cannot be empty";

  String WIDTH_MUST_NOT_BE_BLANK = "Lebar tidak boleh kosong";

  String WIDTH_MUST_NOT_BE_BLANK_EN = "Width cannot be empty";

  String HEIGHT_MUST_NOT_BE_BLANK = "Tinggi tidak boleh kosong";

  String HEIGHT_MUST_NOT_BE_BLANK_EN = "Height cannot be empty";

  String WEIGHT_MUST_NOT_BE_BLANK = "Berat tidak boleh kosong";

  String WEIGHT_MUST_NOT_BE_BLANK_EN = "Weight cannot be empty";

  String DESCRIPTION_MUST_NOT_BE_BLANK = "Deskripsi tidak boleh kosong";

  String DESCRIPTION_MUST_NOT_BE_BLANK_EN = "Description cannot be empty";

  String LENGTH_INVALID = "Panjang tidak sesuai";

  String LENGTH_INVALID_EN = "Length is invalid";

  String WIDTH_INVALID = "Lebar tidak sesuai";

  String WIDTH_INVALID_EN = "Width is invalid";

  String HEIGHT_INVALID = "Tinggi tidak sesuai";

  String HEIGHT_INVALID_EN = "Height is invalid";

  String WEIGHT_INVALID = "Berat tidak sesuai";

  String WEIGHT_INVALID_EN = "Weight is invalid";

  String WRONG_WEIGHT = "Berat harus lebih besar dari 0";

  String WRONG_WEIGHT_EN = "Weight must be greater than 0";

  String MINIMUM_LENGTH_VALUE_INVALID = "Panjang tidak boleh kurang dari 0";

  String MINIMUM_LENGTH_VALUE_INVALID_EN = "The length cannot be less than 0";

  String MINIMUM_WIDTH_VALUE_INVALID = "Lebar tidak boleh kurang dari 0";

  String MINIMUM_WIDTH_VALUE_INVALID_EN = "Width cannot be less than 0";

  String MINIMUM_HEIGHT_VALUE_INVALID = "Tinggi tidak boleh kurang dari 0";

  String MINIMUM_HEIGHT_VALUE_INVALID_EN = "Height cannot be less than 0";

  String PICKUP_POINT_CODE_INVALID = "Pickup point tidak valid";

  String BP_BOPIS_ELIGIBILITY_ERROR = "Anda tidak bisa menggunakan tipe pengiriman ini. Silahkan hubungi Seller Care untuk info lengkap";

  String BOPIS_ELIGIBILITY_ERROR = "Anda tidak bisa menggunakan tipe pengiriman ini. Silahkan hubungi Seller Care untuk info lengkap";

  String BP_BOPIS_ELIGIBILITY_ERROR_EN = "Cannot use this delivery type. Please contact Seller Care for complete information";

  String BOPIS_ELIGIBILITY_ERROR_EN = "Cannot use this delivery type. Please contact Seller Care for complete information";

  String PICKUP_POINT_CODE_INVALID_EN = "Invalid pickup point";

  String INVALID_ROW_BY_PARENT_ID_MESSAGE =
      "Item ini milik orang tua yang sama dari sku item tidak valid. Karenanya item ini tidak perlu diedit";

  String INVALID_ROW_BY_PARENT_ID_MESSAGE_EN =
      "This item belongs to the same parent of invalid item sku. Hence this item need not be edited";

  String UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS_EN = "Max. 400 characters for Unique selling point";

  String UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS = "Maks. 400 karakter untuk Keunggulan produk";

  String IMAGE_SIZE_VALIDATION_ERR_MESSAGE_EN = "Please upload max. 4 MB (for single image) and 20 MB (for .zip file)";

  String IMAGE_MAX_SIZE_VALIDATION_ERR_MESSAGE_EN = "Image size is greater than 4Mb";

  String IMAGE_SIZE_VALIDATION_ERR_MESSAGE = "Silakan upload maks. 4 MB (untuk 1 foto) atau 20 MB (untuk file .zip)";

  String DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS_EN = "Max. 5000 characters for Description";

  String DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS = "Maks. 5000 karakter untuk Deskripsi";

  String IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN = "Image resolution must be minimum 600*600 pixels dimensions";

  String IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE = "Resolusi gambar harus berukuran minimal 600*600 piksel";

  String SELLER_TYPE_ERROR = "Seller type for this item doesn't allow vat flag update";

  String ITEM_OWNED_BY_DIFFERENT_SELLER = "Item requested for update doesn't belong to this seller";

  String ITEM_NOT_FOUND = "Item not found";

  String WARNING_PREFIX = "Warning, unable to parse: ";

  String INSTORE_INPUT_VALIDATION_ERROR = "Blibli product sku or off2on flag value is not proper.";

  String DUPLICATE_PRODUCT_SKUS = "Duplicate product skus found.";

  String INVALID_PRODUCT_SKU = "Invalid product sku or not belong to the seller %s";

  String INVALID_ITEM_SKU_ERROR = "Invalid item sku or not belong to the seller %s";

  String NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND_EN = "This seller doesn't have authorization letter for the selected brand. Please change the brand and try again.";

  String NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND_IN = "Seller ini tidak memiliki surat penunjukan resmi untuk brand terpilih. Silakan ubah brand dan coba lagi.";

  String INVALID_BUSINESS_PARTNER_CODE = "Not an active business partner : ";

  String BRAND_CANNOT_BE_BLANK_EN = "cannot be blank";

  String BRAND_CANNOT_BE_BLANK = "tidak boleh kosong";

  String BUSINESS_PARTNER_CODE_CANNOT_BE_BLANK = "Business Partner Code cannot be blank";

  String STORE_ID_CANNOT_BE_BLANK = "Store id must not be blank";

  String LIST_PRICE_MUST_NOT_BE_BLANK = "Daftar harga tidak boleh kosong atau nol";

  String STOCK_MUST_NOT_BE_BLANK = "Stok tidak boleh kosong";

  String STOCK_MUST_NOT_BE_BLANK_EN = "Stock cannot be empty";

  String DUPLICATE_VARIANT_CREATING_ATTRIBUTE =
    "Varian pada parent yang sama harus berbeda atau diisi";

  String DUPLICATE_VARIANT_CREATING_ATTRIBUTE_EN =
    "Variant under the same parent must be " + "different or filled";

  String CATEGORY_NOT_FOUND_ERROR_MESSAGE = "not found category with codes";

  String CATEGORY_NOT_FOUND_ERROR_MESSAGE_ID = "Kategori tidak ditemukan";

  String CANNOT_ADD_RESTRICTED_KEYWORD_TO_CATEGORY = "Gagal menambahkan kata pencarian untuk kategori";
  String INVALID_ITEM_SKU = "%s is a non CNC Product";

  String ADD_CAMPAIGN_PRODUCT_FAILURE_MESSAGE = "Error in adding the product to campaign";
  String SHIPPING_TYPE_INELIGIBLE = "You are not eligible to use this shipping option. Please "
    + "contact Seller Care for more info.";

  String STOCK_MUST_BE_NUMBER = "Stok harus berupa angka";
  String STOCK_MUST_BE_POSITIVE_VALUE = "Stok harus bernilai positif";
  String REGULAR_PRICE_MUST_BE_DECIMAL = "Harga Normal harus desimal";
  String MPP_NOT_ALLOWED_FOR_SELLER = "Multi pickup point not allowed for this seller.";
  String PICKUP_POINT_NOT_ELIGIBLE_FOR_CNC = "Kode titik pengambilan tidak memenuhi syarat untuk cnc";
  String PRICE_MUST_BE_GREATER_THAN_MINIMUM_VALUE_PRICE = "Harga harus lebih besar dari";
  String OFFER_PRICE_MUST_BE_DECIMAL = "Harga Penawaran harus dalam desimal";
  String FREE_SAMPLE_CANNOT_BE_ONLINE = "Pengiriman Tidak Dapat Online untuk Produk sampel gratis";
  String FREE_SAMPLE_CANNOT_BE_CNC = "Klik & Kumpulkan Tidak Dapat Online untuk mendapatkan "
    + "sampel Produk gratis";
  String PRODUCT_CANT_BE_SET_ONLINE =
      "Cannot make product online , Please add appropriate Dimensions before making product online.";
  String PRODUCT_CANT_BE_SET_CNC =
      "Shipping Type BOPIS is not allowed for CNC product, either select different shipping type or mark CNC as false";
  String DUPLICATE_EAN_UPC_VALUES_INVALID_EN = "EAN/UPC code has been used";
  String DUPLICATE_EAN_UPC_VALUES_INVALID = "Kode EAN/UPC telah digunakan";
  String HEADER_VALIDATION_ERROR = "File tidak valid. Silakan download template baru dan coba lagi.";
  String CHILD_SKU_QUANTITY_MISMATCH_EN = "Quantity must match the child SKU.";
  String CHILD_SKU_QUANTITY_MISMATCH_ID = "Jumlah harus sama dengan SKU child.";
  String CHILD_SKU_MAX_LIMIT_EN = "Max. child SKU: 10.";
  String CHILD_SKU_MAX_LIMIT_ID = "Maks. SKU child: 10.";
  String QUANTITY_INVALID = "Jumlah tidak sesuai";
  String QUANTITY_INVALID_EN = "Quantity is not appropriate";
  String QUANTITY_MUST_BE_GREATER_THAN_MINIMUM_VALUE = "Jumlah min.: 1";
  String QUANTITY_MUST_BE_GREATER_THAN_MINIMUM_VALUE_EN = "Min. quantity: 1";
  String CHILD_SKU_INVALID_EN = "Child sku is invalid";
  String CHILD_SKU_INVALID_ID = "Sku child tidak sesuai";
  String WAREHOUSE_CODE_INVALID_EN = "Warehouse code not found.";
  String WAREHOUSE_CODE_INVALID_ID = "kode gudang tidak ditemukan.";
  String ITEM_NOT_FOUND_EN = "Item not found.";
  String ITEM_NOT_FOUND_ID = "Item tidak ditemukan.";
  String SOURCE_ITEM_NOT_FOUND_EN = "Source item not found.";
  String SOURCE_ITEM_NOT_FOUND_ID = "Asal item tidak ditemukan.";
  String DESTINATION_ITEM_NOT_FOUND_EN = "Destination item not found.";
  String DESTINATION_ITEM_NOT_FOUND_ID = "Tujuan item tidak ditemukan.";
  String NOT_A_BUNDLE_PRODUCT_EN = "Not a bundle product";
  String NOT_A_BUNDLE_PRODUCT_ID = "Bukan produk yang digabungkan.";
  String MINIMUM_STOCK_1_EN = "Min. stock: 1.";
  String MINIMUM_STOCK_1_ID = "Stok min: 1.";
  String BUNDLING_TYPE_INVALID_EN = "Must be physical or virtual.";
  String BUNDLING_TYPE_INVALID_ID = "Harus fisik atau virtual.";
  String COGS_MUST_MATCH_CHILD_SKU_EN = "COGS must match the child SKU.";
  String COGS_MUST_MATCH_CHILD_SKU_ID = "COGS harus sama dengan SKU child.";
  String COGS_SUM_TO_100_EN = "Total COGS must be 100%.";
  String COGS_SUM_TO_100_ID = "Total COGS harus 100%.";
  String CANNOT_PROCESS_INVALID_DATA = "Can not process invalid input data :";
  String PRODUCT_CODE_EMPTY = "Kode produk wajib diisi.";
  String DUPLICATE_PRODUCT_CODE = "Ditemukan kode produk yang sama.";
  String INVALID_REVIEWER = "Reviewer tidak valid.";
  String ITEM_SKU_MUST_NOT_BE_BLANK_EN = "Item Sku cannot be empty. ";
  String PRODUCT_SKU_MUST_NOT_BE_BLANK_EN = "Product Sku cannot be empty. ";
  String SELLER_CODE_MUST_NOT_BE_BLANK_EN = "Seller code cannot be empty. ";
  String INVALID_BULK_PROCESS = "Invalid bulk process";
  String INVALID_MONTH = "Invalid Month. ";
  String INVALID_YEAR = "Invalid Year. ";
  String INVALID_PROJECTED_REBATE = "Invalid Projected Rebate. ";
  String INVALID_OR_BLANK_ITEM_SKU = "ItemSku must not be blank or InValid. ";
  String INVALID_OR_BLANK_DELETE_TYPE_TAGGING = "Delete Tagging is Invalid or is Blank. ";
  String PRODUCT_TYPE_TAGGING_MUST_NOT_BE_BLANK_EN = "Product Type Tagging must not be Blank. ";
  String INVALID_ROW_BY_PARENT_ID_FOR_MERGED_CELL =
      "This item belongs to the same parent of invalid item having merged cells. Hence this item cannot be created";
  String MERGED_CELLS_ERROR = "Cells are merged for this row";
  String DUPLICATE_ROW_PRODUCT_TAGGING_ERROR =
    "Duplicate Rows are not allowed for product tagging upload request";
  String INACCESSIBLE_PICKUP_POINTS = "Gagal menambahkan produk karena Anda tidak memiliki akses untuk alamat pengambilan ini.";
  String INACCESSIBLE_PICKUP_POINTS_EN = "Failed to add product because you don't have access to this pickup point.";
  String ACTION_MUST_NOT_BE_BLANK = "Action must not be blank";
  String MANDATORY_FIELDS_MUST_NOT_BE_BLANK = "Mandatory fields must not be blank";
  String INVALID_ACTION = "Invalid Action";
  String UNKNOWN_SOURCE = "Unknown Source";
  String DATE_NOT_IN_CORRECT_FORMAT = "Date is not in Correct Format. It should be MM/dd/yyyy";
  String UNKNOWN_VIOLATION_TYPE = "Unknown Violation";
  String UNKNOWN_REASONS = "Unknown Reasons";
  String BLANK_SKU_LEVEL_REBATE = "Rebate value must not be blank";
  String BLANK_CAMPAIGN_ID = "Campaign ID must not be blank";
  String DUPLICATE_L5_ROW_ERROR = "Duplicate item sku and pick up point code";
  String SKU_ID_DOES_NOT_BELONG_TO_EMAIL_ADDRESS = "Verify if the SKU belongs to your accessible Stores and Categories.";
  String SELLING_PRICE_UPDATE_SUCCESS = "Normal Price/Selling Price Updated Successfully";
  String CAMPAIGN_PRICE_UPDATE_SUCCESS = "Campaign Price Updated Successfully";
  String RECHECK_CAMPAIGN_CODE_ERROR = "Please verify campaign code and try campaign price update again";
  String SELLING_PRICE_UPDATE_FAILURE = "Normal Price/Selling Price Update failed due to following reason: ";
  String CAMPAIGN_PRICE_UPDATE_FAILURE = "Campaign Price Update failed due to following reason: ";
  String NO_ELIGIBLE_ROWS_DETECTED = "No rows are detected which are eligible for update";
  String PURE_INSTORE_MISSING_FIELDS_ERROR = "Anda bisa ubah status ke Online setelah mengisi"
    + " kolom yang diwajibkan (Deskripsi, Spesifikasi, dan Info pengiriman).";
  String PP_UPDATE_UNSUPPORTED_L5_WAREHOUSE_STOCK_AVAILABLE = "Masih ada stok di alamat pengambilan ini.";

  String L5_DELETE_REQUEST_INVALID_FOR_WAREHOUSE_STOCK =
    "Delete PickupPoint request cannot be processed due to remaining stock on that pickup point.";
  String IMAGE_VALIDATION_FAILED = "Image validation failed";
  String VIDEO_VALIDATION_FAILED = "Video validation failed";
  String INVALID_YOUTUBE_URL = "Link youtube tidak valid atau tidak dapat diakses";
  String VIDEO_URL_MUST_BE_YOUTUBE = "Link video harus merupakan youtube URL";

  // Basic info validation error messages
  String INSTORE_CANNOT_BE_BLANK = "Instore cannot be blank";
  String INSTORE_MUST_BE_NUMERIC = "Instore should be number";
  String MAIN_IMAGE_MUST_NOT_BE_BLANK = "Main image must not be blank";
  String PRODUCT_NOT_FOUND = "Product is not active";
  String ERROR_WHILE_SCALING = "Error while scaling image %s.";
  String PRODUCT_TAKEN_DOWN = "Produk telah dinonaktifkan.";
  String PRODUCT_DELETED = "Produk telah dihapus.";
  String PRODUCT_ARCHIVED = "Produk telah diarsipkan.";
  String PRODUCT_PROCESS_FAILED =
      "Gagal memroses produk. Silakan ubah dan coba lagi.";
  String PRODUCT_NOT_BELONG_TO_BUSINESS_PARTNER =
      "Produk bukan bagian dari partner bisnis terpilih.";
  String INVALID_DORMANT_SELLER_PRODUCT = "Invalid Dormant Seller Product";
  String NO_CHANGE_IN_EAN_UPC = "Tidak ada perubahan. EAN/UPC yang dimasukkan sama seperti sebelumnya.";
  String BULK_UPDATE_INVALID_EAN_LENGTH = "EAN/UPC harus terdiri dari 5, 8, 12, 13, 14, atau 15 karakter.";
  String BULK_UPDATE_EAN_INPUT_ERROR = "Format tidak valid. EAN/UPC hanya boleh berisi angka.";
}
