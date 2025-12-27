package com.gdn.partners.bulk.util;

public interface BulkCnCreationHeaderNames {
   String BRAND_HEADER_NAME = "Brand";

   String PRODUCT_NAME_EN_HEADER_NAME = "Product Name";
   String PRODUCT_NAME_ID_HEADER_NAME = "Nama Produk";

   String PRODUCT_DESCRIPTION_EN_HEADER_NAME = "Description";
   String PRODUCT_DESCRIPTION_ID_HEADER_NAME = "Deskripsi";

   String PRODUCT_USP_EN_HEADER_NAME = "Unique Selling Point";
   String PRODUCT_USP_ID_HEADER_NAME = "Keunggulan produk";

   String PRODUCT_DELIVERY_STATUS_EN_HEADER_NAME = "Status/Shipping";
   String PRODUCT_DELIVERY_STATUS_ID_HEADER_NAME = "Status/Pengiriman";

   String PRODUCT_CNC_STATUS_HEADER_NAME = "Click & Collect";

   String PRODUCT_TYPE_EN_HEADER_NAME = "Handling Type";
   String PRODUCT_TYPE_ID_HEADER_NAME = "Tipe Penanganan";

   String NORMAL_PRICE_EN_HEADER_NAME = "Normal Price (Rp)";
   String NORMAL_PRICE_ID_HEADER_NAME = "Normal Harga (Rp)";

   String SELLING_PRICE_EN_HEADER_NAME = "Selling Price (Rp)";
   String SELLING_PRICE_ID_HEADER_NAME = "Harga Penjualan (Rp)";

   String AVAILABLE_STOCK_EN_HEADER_NAME = "Available Stock";
   String AVAILABLE_STOCK_ID_HEADER_NAME = "Stok Tersedia";

   String MINIMUM_STOCK_EN_HEADER_NAME = "Minimum Stock";
   String MINIMUM_STOCK_ID_HEADER_NAME = "Stok Minimum";

   String LENGTH_EN_HEADER_NAME = "Length (cm)";
   String LENGTH_ID_HEADER_NAME = "Panjang (cm)";

   String WIDTH_EN_HEADER_NAME = "Width (cm)";
   String WIDTH_ID_HEADER_NAME = "Lebar (cm)";

   String HEIGHT_EN_HEADER_NAME = "Height (cm)";
   String HEIGHT_ID_HEADER_NAME = "Tinggi (cm)";

   String WEIGHT_EN_HEADER_NAME = "Weight (gram)";
   String WEIGHT_ID_HEADER_NAME = "Berat (gram)";

   String IMAGE_EN_HEADER_NAME = "Image-";
   String IMAGE_ID_HEADER_NAME = "Foto-";

   String FIRST_IMAGE_EN_HEADER_NAME = "Image-1";
   String FIRST_IMAGE_ID_HEADER_NAME = "Foto-1";

   String PICKUP_POINT_CODE_EN_HEADER = "Shop/Warehouse Code";
   String PICKUP_POINT_CODE_ID_HEADER = "Kode Toko/Gudang";

   String EAN_UPC_CODE_HEADER = "Model/EAN/UPC";

   String YOUTUBE_URL_HEADER = "Url Video";

   String GENERATED_ITEM_NAME = "generatedItemName";

   String SELLER_SKU = "Seller SKU";

   String ROW_NUMBER = "rowNumber";

   String BULK_PROCESS_NOTES = "bulkProcessNotes";

   String BFB_MANAGED_HEADER_ID = "BFB Diatur";

   String BFB_MANAGED_HEADER_EN = "Managed";

   String BFB_STATUS_HEADER = "BfB Status";

   String BFB_BASE_PRICE_HEADER_EN = "Base Price";

   String BFB_BASE_PRICE_HEADER_ID = "Harga dasar";

   String CHILD_SKU_EN = "Child SKU";

   String QUANTITY_EN = "Quantity";

   String CHILD_SKU_ID = "SKU child";

   String QUANTITY_ID = "Jumlah";
   String VARIANT_IMAGE_EN = "Variant photo";
   String VARIANT_IMAGE_ID = "Foto varian";
   String INSTORE = "Instore Status";
   String INSTORE_DESC_EN = "Please fill with number. " + "Online = 1, Offline = 0"
       + " If one of the variant have" + " instore status online, all" + " variant in that product will"
       + " be instore online as well";
   String INSTORE_DESC_ID = "Silakan isi dengan angka. " + "Online = 1, Offline = 0"
       + " Jika salah satu variant memiliki status online, maka seluruh variant akan menjadi online untuk instore";
}
