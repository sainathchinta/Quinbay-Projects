package com.gdn.partners.pcu.external.model;

public interface ExcelHeaderNames {
  String BLIBLI_SKU = "Blibli SKU";
  String BLIBLI_SKU_DESCRIPTION = "Diisi dengan kode SKU varian dari sistem Blibli. Contoh:";
  String BLIBLI_SKU_EN = "Blibli SKU (Item SKU)";
  String PICKUP_POINT_CODE_EN = "Pickup point code";
  String PICKUP_POINT_NAME_EN="Pickup point name";
  String PICKUP_POINT_CODE_SHEET_EN="Pickup point";
  String PICKUP_POINT_CODE_SHEET_ID="Alamat pengambilan";
  String SKU_BLIBLI = "SKU Blibli (SKU item)";
  String NAMA_PRODUK = "Nama Produk";
  String NAMA_PRODUK_DESCRIPTION = "Diisi dengan nama varian produk. Tidak akan mengubah data yang ada saat ini";
  String HARGA_NORMAL = "Harga normal";
  String HARGA_JUAL = "Harga jual";
  String SKU_CODE = "SKU Code";
  String SKU_CODE_DESCRIPTION = "Diisi dengan kode produk. Contoh: MTA-123456789-00001";
  String SELLER_SKU = "Seller SKU";
  String SELLER_SKU_DESCRIPTION = "Diisi dengan SKU seller. Bisa diedit sesuai varian.";
  String PICKUPPOINT_CODE_ID = "Kode alamat pengambilan";
  String PICKUPPOINT_NAME_ID = "Nama alamat pengambilan";
  String HARGA_RP = "Harga (Rp)";
  String HARGA_RP_DESCRIPTION= "Diisi dengan harga normal sebelum diskon. Hanya boleh diisi angka. "
      + "Harga produk promo tidak bisa diubah.";
  String HARGA_PENJUALAN_RP = "Harga Penjualan (Rp)";
  String HARGA_PENJUALAN_RP_DESCRIPTION = "Diisi dengan harga jual sebelum diskon. Hanya boleh diisi angka. "
      + "Harga produk promo tidak bisa diubah.";
  String STOK = "Stok";
  String STOK_DESCRIPTION = "Diisi dengan stok saat ini. Hanya boleh diisi angka.";
  String PO_QUOTA = "PO Quota";
  String PO_QUOTA_DESCRIPTION = "Diisi dengan quota saat ini. Hanya boleh diisi angka.";
  String STOCK = "Stock";
  String TOKO_GUDANG = "Toko/Gudang";
  String TOKO_GUDANG_DESCRIPTION =
      "Pilih alamat pengambilan dari opsi yang ada. Contoh: PP-3212345. Kode yang sudah ada tidak bisa diubah, hanya bisa menambahkan kode alamat pengambilan baru.";
  String DITAMPILKAN = "Ditampilkan";
  String DAPAT_DIBELI = "Dapat Dibeli";
  String OFFLINE_TO_ONLINE = "Offline To Online";
  String WAREHOUSE_STOCK = "Warehouse Stock";
  String WAREHOUSE_STOCK_DESCRIPTION = "Stok gudang saat ini. " + "Tidak akan mengubah data yang ada saat ini.";
  String NAMA_PRODUCK = "Nama Produk";
  String URL_VIDEO = "Url Video";
  String TIPE_PENANGAN = "Tipe Penanganan";
  String HARGA = "Normal Harga (Rp)";
  String HARGA_PENJULAN = "Harga Penjualan (Rp)";
  String AVAILABLE_STOCK = "Available Stock";
  String MINIMUM_STOCK = "Minimum Stock";
  String KODE_PICKUP_POINT = "Kode Toko/Gudang";
  String PANJANG = "Panjang (cm)";
  String LEBAR = "Lebar (cm)";
  String TINGGI = "Tinggi (cm)";
  String BERAT = "Berat (gram)";
  String DESKRIPSI = "Deskripsi";
  String STATUS_PENGIRIMAN = "Status/Pengiriman";
  String UNIQUE_SELLING_POINT = "Unique Selling Point";
  String BUYABLE = "Buyable";
  String MELALUI_PARTNER_LOGISTIK_BLIBLI = "Melalui partner logistik Blibli";
  String DIKIRIMKAN_OLEH_SELLER = "Dikirimkan oleh seller";
  String KEUNGGULAN_PRODUK = "Keunggulan produk";
  String BISA_DIBELI= "Bisa Dibeli";
  String STOK_TERSEDIA = "Stok Tersedia";
  String STOK_MINIMUM = "Stok Minimum";
  String FOTO_PREFIX = "Foto-";

  //for international merchant

  String PRODUCT_NAME = "Product Name";
  String DESCRIPTION = "Description";
  String HANDLING_TYPE = "Handling Type";
  String PICKUP_POINT_CODE = "Shop/Warehouse Code";
  String LENGTH = "Length (cm)";
  String WIDTH = "Width (cm)";
  String HEIGHT = "Height (cm)";
  String WEIGHT = "Weight (gram)";
  String PRICE = "Normal Price (Rp)";
  String SELLING_PRICE = "Selling Price (Rp)";
  String IMAGE_PREFIX = "Image-";
  String MODEL_EAN_UPC = "Model/EAN/UPC";
  String PARENT = "Parent";
  String DELIVERY_STATUS = "Status/Shipping";
  String CNC = "Click & Collect";
  String THROUGH_BLIBLI_LOGISTIC_PARTNER = "Through Blibli logistic partner";
  String SHIPPED_BY_SELLER = "Shipped by seller";
  String BOPIS = "Bopis";
  String BLIBLI_PRODUCT_SKU = "Blibli Product SKU";
  String BLIBLI_PRODUCT_SKU_DESCRIPTION =
      "Diisi dengan kode SKU produk dari sistem Blibli. Contoh: BEE-12345-00001";
  String IN_STORE_HEADER = "Instore";
  String PARENT_PRODUCT_NAME = "Parent Product name";
  String PARENT_PRODUCT_NAME_DESCRIPTION =
      "Diisi dengan nama produk parent. Tidak akan mengubah data yang ada saat ini.";
  String AMPHI_SKU_STATUS = "Status/pengiriman(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)";
  String AMPHI_SKU_STATUS_DESCRIPTION = "Diisi dengan angka sesuai status produk: " + "0 = Offline, 1 = Online, "
      + "2 = Akan datang, 3 = Dibeli via link "
      + "Untuk status 1, 2, dan 3, aktifkan pengiriman atau Click and Collect";
  String EXTERNAL_SKU_STATUS = "Status/pengiriman(0 = Offline, 1 = Online)";
  String EXTERNAL_SKU_STATUS_DESCRIPTION = "Diisi dengan angka sesuai status produk: "
      + "0 = Offline, 1 = Online";
  String EXTERNAL_SKU_STATUS_EN = "Status/Shipping(0 = Offline, 1 = Online)";
  String SKU_STATUS_OFFLINE = "0";
  String SKU_STATUS_ONLINE = "1";
  String SKU_STATUS_TEASER = "2";
  String SKU_STATUS_B2B = "3";
  String IN_STORE = "Instore";
  String IN_STORE_DESCRIPTION = "Status Instore saat ini: " + "0 = Offline, 1 = Online. "
      + "Tidak akan mengubah data yang ada saat ini.";
  String CNC_ACTIVE = "Click & Collect(0= Off, 1 = On)";
  String CNC_ACTIVE_DESCRIPTION = "Diisi dengan angka sesuai status Click & Collect: " + "0 = Offline, 1 = Online";
  String DELIVERY_ACTIVE = "Pengiriman(0 = Offline, 1 = Online)";
  String DELIVERY_ACTIVE_NEW = "Pengiriman(0 = Off, 1 = On)";
  String DELIVERY_ACTIVE_NEW_DESCRIPTION = "Diisi dengan angka sesuai status pengiriman: "
      + "0 = Offline, 1 = Online";
  String DELIVERY_ACTIVE_EN_NEW = "Delivery(0 = Offline, 1 = Online)";
  String EXTERNAL_SKU_STATUS_NEW = "Status(0 = Offline, 1 = Online)";
  String AMPHI_SKU_STATUS_NEW = "Status(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)";

  //for Brand Auth
  String BRAND_CODE = "Brand Code";
  String SELLER_CODE = "Seller Code";
  String BRAND_NAME = "Brand Name";
  String SELLER_NAME = "Seller Name";
  String AUTH_START_DATE = "Authorisation Start date(DD/MM/YYYY)";
  String AUTH_END_DATE = "Authorisation End date(DD/MM/YYYY)";
  String BFB_MANAGED_ID = "BFB Diatur";
  String BFB_MANAGED_EN = "Managed";
  String BFB_STATUS = "BfB Status";
  String BASE_PRICE_EN = "Base Price";
  String BASE_PRICE_ID = "Harga dasar";
  String BFB_BASE_PRICE = "Bfb Harga dasar";
  String BFB_BASE_PRICE_DESCRIPTION = "Diisi dengan harga jual sebelum diskon. Hanya boleh diisi angka. ";
  String AMPHI_BFB_STATUS = "Bfb Status(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)";
  String EXTERNAL_BFB_STATUS = "Bfb Status(0 = Offline, 1 = Online)";
  String EXTERNAL_BFB_STATUS_DESCRIPTION = "Diisi dengan angka sesuai status BFB: " + "0 = Offline, 1 = Online";
  String BFB_MANAGED = "Bfb Diatur(1 = Yes, 0=No)";
  String BFB_MANAGED_DESCRIPTION = "Diisi dengan angka sesuai pengaturan BFB: " + "0 = Tidak aktif, 1 = Online";
  String BFB_MANAGED_TRUE = "1";
  String BFB_MANAGED_FALSE = "0";
  String VARIANT_IMAGE_EN = "Variant photo";
  String VARIANT_IMAGE_ID = "Foto varian";
  String INSTORE_EN = "Instore Status";
  String PICKUP_POINT_NAME_COLUMN_ID = "Nama alamat pengambilan";
  String PICKUP_POINT_NAME_COLUMN_DESCRIPTION =
      "Diisi dengan nama alamat pengambilan sesuai dengan kode alamat pengambilan. ";
  String STOCK_REMINDER_COLUMN_ID = "Pengingat stok";
  String STOCK_REMINDER_COLUMN_DESCRIPTION_ID = "Diisi dengan angka 0, 1, 2 dst untuk mendapatkan pengingat stok."
      + "Diisi dengan Tidak aktif atau kosongkan jika tidak ingin menerima pengingat stok. ";
}
