# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════

# build dept tabs at UI eval time (DEPARTMENTS already loaded by global.R)
dept_tab_items <- unlist(lapply(DEPARTMENTS, function(dept) {
  env <- new.env(parent = globalenv())
  source(file.path(dept$dir, "ui_tabs.R"), local = env)
  env$dept_ui_tabs
}), recursive = FALSE)

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700;800&display=swap"),
    tags$style(HTML("
    /* ══════════════════════════════════════════════════════════
       ASYV MODERN DESIGN SYSTEM
       ══════════════════════════════════════════════════════════ */
    :root{
      /* ── ASYV Brand Palette ─────────────────────────────── */
      --asyv-forest:    #0B4D3B;    /* deep forest green — primary */
      --asyv-forest-d:  #083D2F;    /* darker shade */
      --asyv-forest-l:  #0E6B52;    /* lighter shade */
      --asyv-emerald:   #10B981;    /* vibrant emerald accent */
      --asyv-emerald-l: #D1FAE5;    /* soft emerald bg */
      --asyv-sage:      #6EE7B7;    /* sage highlight */

      --asyv-navy:      #0F172A;    /* deep navy — secondary */
      --asyv-navy-mid:  #1E293B;
      --asyv-slate:     #334155;

      --asyv-gold:      #D4A843;    /* warm gold — tertiary */
      --asyv-gold-l:    #F3E8C8;
      --asyv-gold-d:    #B8922F;

      --asyv-terracotta:#C75B39;    /* earthy terracotta */
      --asyv-amber:     #F59E0B;

      /* ── Neutrals ──────────────────────────────────────── */
      --bg:             #F8FAFC;
      --bg-warm:        #FAFAF8;
      --surface:        #FFFFFF;
      --surface-raised: #FFFFFF;
      --border:         #E2E8F0;
      --border-light:   #F1F5F9;
      --text:           #1E293B;
      --text-secondary: #64748B;
      --text-muted:     #94A3B8;

      /* ── Shadows ───────────────────────────────────────── */
      --shadow-xs:  0 1px 2px rgba(15,23,42,.04);
      --shadow-sm:  0 1px 3px rgba(15,23,42,.06), 0 1px 2px rgba(15,23,42,.04);
      --shadow-md:  0 4px 6px -1px rgba(15,23,42,.07), 0 2px 4px -2px rgba(15,23,42,.05);
      --shadow-lg:  0 10px 15px -3px rgba(15,23,42,.08), 0 4px 6px -4px rgba(15,23,42,.04);
      --shadow-xl:  0 20px 25px -5px rgba(15,23,42,.1), 0 8px 10px -6px rgba(15,23,42,.06);
      --shadow-glow:0 0 20px rgba(16,185,129,.15);

      /* ── Geometry ──────────────────────────────────────── */
      --radius-sm:  6px;
      --radius:     10px;
      --radius-lg:  14px;
      --radius-xl:  18px;
      --radius-2xl: 24px;
      --transition: all .2s cubic-bezier(.4,0,.2,1);
    }

    /* ══════ BASE ═════════════════════════════════════════════ */
    /* Set font on body + structural elements, NOT via * !important
       so Font Awesome icon fonts (.fas/.far/.fab) can override through specificity */
    body,
    h1, h2, h3, h4, h5, h6,
    p, a, span, div, label,
    input, textarea, select, button,
    td, th, li,
    .content-wrapper, .main-sidebar, .main-header,
    .main-footer, .card, .card-body, .card-header,
    .form-control, .btn, .navbar, .sidebar-menu,
    table, .dataTables_wrapper {
      font-family: 'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif !important;
    }
    body{ background: var(--bg) !important; color: var(--text) !important; }
    *{ -webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; }

    /* ══════ NAVBAR ═══════════════════════════════════════════ */
    .main-header .navbar{
      background: linear-gradient(135deg, var(--asyv-navy) 0%, var(--asyv-forest-d) 100%) !important;
      box-shadow: 0 1px 3px rgba(0,0,0,.18), 0 4px 20px rgba(11,77,59,.2) !important;
      border-bottom: none !important;
      min-height: 56px !important;
      backdrop-filter: blur(12px) !important;
    }
    .main-header .navbar .nav-link,
    .main-header .navbar .navbar-nav .nav-link{
      color: rgba(255,255,255,.78) !important;
      transition: var(--transition) !important;
    }
    .main-header .navbar .nav-link:hover{ color: #fff !important; }
    .navbar-badge{ background: var(--asyv-gold) !important; color: var(--asyv-navy) !important; }

    /* ══════ BRAND / SIDEBAR HEADER ══════════════════════════ */
    .brand-link, .brand-link:hover{
      background: var(--asyv-navy) !important;
      border-bottom: 1px solid rgba(255,255,255,.06) !important;
      padding: 14px 18px !important;
    }
    .brand-text{
      color: #fff !important; font-weight: 700 !important;
      font-size: 14.5px !important; letter-spacing: .2px !important;
    }

    /* ══════ SIDEBAR ═════════════════════════════════════════ */
    .main-sidebar, .sidebar{
      background: var(--surface) !important;
    }
    .main-sidebar{
      border-right: 1px solid var(--border-light) !important;
      box-shadow: 1px 0 8px rgba(15,23,42,.04) !important;
    }
    .nav-sidebar > .nav-item{ margin: 3px 12px !important; }
    .nav-sidebar > .nav-item > .nav-link{
      color: var(--text-secondary) !important;
      border-radius: var(--radius) !important;
      padding: 10px 14px !important;
      font-size: 13px !important;
      font-weight: 500 !important;
      transition: var(--transition) !important;
      letter-spacing: .01em !important;
    }
    .nav-sidebar > .nav-item > .nav-link .nav-icon{
      color: var(--asyv-emerald) !important;
      width: 1.35rem !important;
      opacity: .7;
      transition: var(--transition) !important;
    }
    .nav-sidebar > .nav-item > .nav-link:hover{
      background: var(--asyv-emerald-l) !important;
      color: var(--asyv-forest) !important;
    }
    .nav-sidebar > .nav-item > .nav-link:hover .nav-icon{
      color: var(--asyv-forest) !important; opacity: 1;
    }
    .nav-sidebar > .nav-item > .nav-link.active{
      background: linear-gradient(135deg, var(--asyv-forest) 0%, var(--asyv-forest-l) 100%) !important;
      color: #fff !important;
      box-shadow: 0 2px 8px rgba(11,77,59,.3), 0 1px 3px rgba(11,77,59,.2) !important;
      font-weight: 600 !important;
    }
    .nav-sidebar > .nav-item > .nav-link.active .nav-icon{
      color: rgba(255,255,255,.9) !important; opacity: 1;
    }

    /* ══════ CONTENT ═════════════════════════════════════════ */
    .content-wrapper{ background: var(--bg) !important; }
    .content{ padding: 24px 28px !important; }

    /* ══════ CARDS ════════════════════════════════════════════ */
    .card{
      border: 1px solid var(--border-light) !important;
      border-radius: var(--radius-lg) !important;
      box-shadow: var(--shadow-sm) !important;
      transition: box-shadow .25s ease, transform .25s ease !important;
      margin-bottom: 22px !important;
      overflow: hidden !important;
      background: var(--surface) !important;
    }
    .card:hover{
      box-shadow: var(--shadow-md) !important;
      transform: translateY(-1px) !important;
    }
    .card-header{
      background: var(--surface) !important;
      border-bottom: 1px solid var(--border-light) !important;
      padding: 16px 20px !important;
      font-size: 13.5px !important;
      font-weight: 700 !important;
      color: var(--text) !important;
      letter-spacing: .01em !important;
    }
    .card > .card-header > .card-title{
      font-size: 13.5px !important; font-weight: 700 !important;
    }
    .card-body{ padding: 20px !important; }

    /* Card with top accent stripe */
    .card-primary:not(.card-outline) > .card-header{
      background: var(--surface) !important;
      color: var(--text) !important;
      border-top: 3px solid var(--asyv-forest) !important;
      border-bottom: 1px solid var(--border-light) !important;
    }
    .card-success:not(.card-outline) > .card-header{
      background: var(--surface) !important;
      color: var(--text) !important;
      border-top: 3px solid var(--asyv-emerald) !important;
      border-bottom: 1px solid var(--border-light) !important;
    }
    .card-info:not(.card-outline) > .card-header{
      background: var(--surface) !important;
      color: var(--text) !important;
      border-top: 3px solid var(--asyv-gold) !important;
      border-bottom: 1px solid var(--border-light) !important;
    }

    /* ══════ INFO BOXES ══════════════════════════════════════ */
    .info-box{
      border-radius: var(--radius-lg) !important;
      box-shadow: var(--shadow-sm) !important;
      border: 1px solid var(--border-light) !important;
      transition: var(--transition) !important;
      min-height: 82px !important;
      overflow: hidden !important;
      background: var(--surface) !important;
    }
    .info-box:hover{
      transform: translateY(-2px) !important;
      box-shadow: var(--shadow-lg) !important;
    }
    .info-box-icon{
      border-radius: var(--radius-lg) 0 0 var(--radius-lg) !important;
      width: 72px !important;
    }
    .info-box .info-box-icon{
      opacity: .9;
    }
    .info-box-text{
      font-size: 11.5px !important; font-weight: 600 !important;
      text-transform: uppercase !important; letter-spacing: .5px !important;
      color: var(--text-muted) !important;
    }
    .info-box-number{
      font-size: 22px !important; font-weight: 800 !important;
      color: var(--text) !important;
    }

    /* ══════ VALUE BOXES ═════════════════════════════════════ */
    .small-box{
      border-radius: var(--radius-lg) !important;
      box-shadow: var(--shadow-sm) !important;
      transition: var(--transition) !important;
      border: none !important;
      overflow: hidden !important;
    }
    .small-box:hover{
      transform: translateY(-2px) !important;
      box-shadow: var(--shadow-lg) !important;
    }
    .small-box > .inner{ padding: 16px 20px !important; }
    .small-box h3{ font-size: 24px !important; font-weight: 800 !important; }
    .small-box p{ font-size: 12.5px !important; font-weight: 500 !important; opacity: .92; }
    .small-box .icon > i{ font-size: 55px !important; opacity: .15 !important; }

    /* Value-box custom gradient backgrounds */
    .bg-primary, .small-box.bg-primary{
      background: linear-gradient(135deg, var(--asyv-forest) 0%, var(--asyv-forest-l) 100%) !important;
    }
    .bg-success, .small-box.bg-success{
      background: linear-gradient(135deg, #059669 0%, var(--asyv-emerald) 100%) !important;
    }
    .bg-info, .small-box.bg-info{
      background: linear-gradient(135deg, #0369A1 0%, #0EA5E9 100%) !important;
    }
    .bg-warning, .small-box.bg-warning{
      background: linear-gradient(135deg, var(--asyv-gold-d) 0%, var(--asyv-gold) 100%) !important;
      color: var(--asyv-navy) !important;
    }
    .bg-warning h3, .bg-warning p{ color: var(--asyv-navy) !important; }
    .bg-danger, .small-box.bg-danger{
      background: linear-gradient(135deg, #B91C1C 0%, #EF4444 100%) !important;
    }

    /* info-box icon gradient overrides */
    .info-box-icon.bg-primary{
      background: linear-gradient(135deg, var(--asyv-forest) 0%, var(--asyv-forest-l) 100%) !important;
    }
    .info-box-icon.bg-success{
      background: linear-gradient(135deg, #059669 0%, var(--asyv-emerald) 100%) !important;
    }
    .info-box-icon.bg-info{
      background: linear-gradient(135deg, #0369A1 0%, #0EA5E9 100%) !important;
    }
    .info-box-icon.bg-warning{
      background: linear-gradient(135deg, var(--asyv-gold-d) 0%, var(--asyv-gold) 100%) !important;
    }

    /* ══════ FOOTER ══════════════════════════════════════════ */
    .main-footer{
      background: var(--surface) !important;
      border-top: 1px solid var(--border-light) !important;
      font-size: 12px !important;
      color: var(--text-muted) !important;
      padding: 12px 28px !important;
    }

    /* ══════ NAVBAR RIGHT (user badge) ═══════════════════════ */
    .user-badge{
      display: inline-flex !important; align-items: center !important;
      gap: 8px !important; padding: 5px 14px !important;
      border-radius: 24px !important;
      background: rgba(255,255,255,.08) !important;
      color: rgba(255,255,255,.92) !important;
      font-size: 12.5px !important;
      border: 1px solid rgba(255,255,255,.12) !important;
      font-weight: 500 !important;
      backdrop-filter: blur(8px) !important;
    }
    .role-pill{
      background: var(--asyv-gold) !important; color: var(--asyv-navy) !important;
      font-size: 9.5px !important; padding: 2px 9px !important;
      border-radius: 12px !important; font-weight: 700 !important;
      text-transform: uppercase !important; letter-spacing: .5px !important;
    }
    .role-pill.viewer{
      background: var(--asyv-emerald) !important; color: #fff !important;
    }
    #logout_btn{
      background: rgba(255,255,255,.08) !important;
      border: 1px solid rgba(255,255,255,.15) !important;
      color: rgba(255,255,255,.85) !important;
      border-radius: var(--radius) !important;
      padding: 6px 12px !important;
      transition: var(--transition) !important;
      margin-left: 6px !important;
      font-size: 13px !important;
    }
    #logout_btn:hover{
      background: rgba(255,255,255,.18) !important;
      color: #fff !important;
    }

    /* ══════ FORMS & INPUTS ══════════════════════════════════ */
    .form-control{
      border-radius: var(--radius) !important;
      border: 1.5px solid var(--border) !important;
      font-size: 13.5px !important;
      padding: 8px 13px !important;
      transition: var(--transition) !important;
      background: var(--surface) !important;
      color: var(--text) !important;
    }
    .form-control:focus{
      border-color: var(--asyv-emerald) !important;
      box-shadow: 0 0 0 3px rgba(16,185,129,.12) !important;
      outline: none !important;
    }
    .selectize-input{
      border-radius: var(--radius) !important;
      border: 1.5px solid var(--border) !important;
      font-size: 13.5px !important;
      box-shadow: none !important;
    }
    .selectize-input.focus{
      border-color: var(--asyv-emerald) !important;
      box-shadow: 0 0 0 3px rgba(16,185,129,.12) !important;
    }
    .selectize-dropdown{
      border-radius: var(--radius) !important;
      border: 1px solid var(--border) !important;
      box-shadow: var(--shadow-lg) !important;
      margin-top: 4px !important;
    }
    .selectize-dropdown .option:hover,
    .selectize-dropdown .active{
      background: var(--asyv-emerald-l) !important;
      color: var(--asyv-forest) !important;
    }

    .btn{
      border-radius: var(--radius) !important;
      font-size: 13px !important;
      font-weight: 600 !important;
      padding: 8px 16px !important;
      transition: var(--transition) !important;
      letter-spacing: .01em !important;
    }
    .btn-success{
      background: var(--asyv-forest) !important;
      border-color: var(--asyv-forest) !important;
    }
    .btn-success:hover{
      background: var(--asyv-forest-l) !important;
      border-color: var(--asyv-forest-l) !important;
      box-shadow: var(--shadow-md) !important;
    }
    .btn-primary{
      background: var(--asyv-navy-mid) !important;
      border-color: var(--asyv-navy-mid) !important;
    }
    .btn-primary:hover{
      background: var(--asyv-slate) !important;
      box-shadow: var(--shadow-md) !important;
    }
    .btn-danger{
      background: #DC2626 !important;
      border-color: #DC2626 !important;
    }
    .btn-danger:hover{
      background: #EF4444 !important;
      box-shadow: var(--shadow-md) !important;
    }
    .btn-warning{
      background: var(--asyv-gold) !important;
      border-color: var(--asyv-gold) !important;
      color: var(--asyv-navy) !important;
    }
    .btn-warning:hover{
      background: var(--asyv-gold-d) !important;
      box-shadow: var(--shadow-md) !important;
    }

    /* ══════ DATATABLES ══════════════════════════════════════ */
    table.dataTable{
      border-collapse: separate !important;
      border-spacing: 0 !important;
    }
    table.dataTable thead th{
      background: var(--bg) !important;
      color: var(--text-secondary) !important;
      font-weight: 700 !important;
      font-size: 11.5px !important;
      border-bottom: 2px solid var(--border) !important;
      letter-spacing: .4px !important;
      text-transform: uppercase !important;
      padding: 12px 14px !important;
    }
    table.dataTable tbody td{
      padding: 10px 14px !important;
      font-size: 13px !important;
      border-bottom: 1px solid var(--border-light) !important;
      vertical-align: middle !important;
    }
    table.dataTable tbody tr:hover td{
      background: rgba(16,185,129,.04) !important;
    }
    .dataTables_wrapper .dataTables_filter input,
    .dataTables_wrapper .dataTables_length select{
      border-radius: var(--radius-sm) !important;
      border: 1.5px solid var(--border) !important;
      padding: 5px 10px !important;
    }

    /* ══════ SCROLLBAR ═══════════════════════════════════════ */
    ::-webkit-scrollbar{ width: 5px; height: 5px; }
    ::-webkit-scrollbar-track{ background: transparent; }
    ::-webkit-scrollbar-thumb{ background: rgba(15,23,42,.12); border-radius: 4px; }
    ::-webkit-scrollbar-thumb:hover{ background: rgba(15,23,42,.24); }

    /* ══════ FLUID ROW SPACING ════════════════════════════════ */
    .row{ margin-left: -10px !important; margin-right: -10px !important; }
    .row > [class*='col-']{ padding-left: 10px !important; padding-right: 10px !important; }

    /* ══════ ROLE & STATUS BADGES ════════════════════════════ */
    .role-admin{
      color: var(--asyv-gold-d) !important;
      font-weight: 700 !important;
    }
    .role-viewer{
      color: var(--asyv-forest-l) !important;
      font-weight: 700 !important;
    }
    .status-active{ color: #059669 !important; font-weight: 600 !important; }
    .status-inactive{ color: #DC2626 !important; font-weight: 600 !important; }
    hr{ border-color: var(--border-light) !important; }

    /* ══════ PLOTLY CONTAINERS ═══════════════════════════════ */
    .plotly .modebar{ opacity: 0; transition: opacity .2s ease; background: transparent !important; }
    .plotly:hover .modebar{ opacity: 1; }
    .modebar-group{ background: transparent !important; padding: 0 !important; }
    .modebar-btn{ color: var(--text-muted) !important; }
    .modebar-btn:hover, .js-plotly-plot .plotly .modebar-btn:hover{
      color: var(--asyv-forest) !important;
      background: var(--asyv-emerald-l) !important;
      border-radius: 4px !important;
    }

    /* ══════ CARD TOOLS (collapse/expand buttons) ════════════ */
    .card-tools .btn{ background: transparent !important; border: none !important;
                      color: var(--text-muted) !important; padding: 4px 6px !important;
                      box-shadow: none !important; }
    .card-tools .btn:hover{ color: var(--asyv-forest) !important; background: var(--asyv-emerald-l) !important; }

    /* ══════ TAB PANE ═════════════════════════════════════════ */
    .tab-pane{ animation: fadeIn .18s ease; }
    @keyframes fadeIn{ from{ opacity: 0; transform: translateY(6px); } to{ opacity: 1; transform: translateY(0); } }

    /* ══════ ABOUT SECTION ═══════════════════════════════════ */
    .card-body p{ font-size: 13.5px; line-height: 1.7; color: var(--text-secondary); }

    /* ══════ USER MANAGEMENT ═════════════════════════════════ */
    .um-stat-card{
      display: flex; align-items: center; gap: 16px;
      background: var(--surface); border-radius: var(--radius-lg);
      border: 1px solid var(--border-light); box-shadow: var(--shadow-sm);
      padding: 18px 22px; margin-bottom: 20px; transition: var(--transition);
    }
    .um-stat-card:hover{ box-shadow: var(--shadow-md); transform: translateY(-2px); }
    .um-stat-icon{
      width: 50px; height: 50px; border-radius: var(--radius);
      display: flex; align-items: center; justify-content: center;
      font-size: 20px; flex-shrink: 0;
    }
    .um-icon-forest{ background: var(--asyv-emerald-l); color: var(--asyv-forest); }
    .um-icon-green{ background: #D1FAE5; color: #059669; }
    .um-icon-gold{ background: var(--asyv-gold-l); color: var(--asyv-gold-d); }
    .um-stat-label{
      font-size: 11px; font-weight: 700; text-transform: uppercase;
      letter-spacing: .55px; color: var(--text-muted); margin-bottom: 4px;
    }
    .um-stat-value{ font-size: 28px; font-weight: 800; color: var(--text); letter-spacing: -.03em; line-height: 1; }
    .um-card{
      background: var(--surface); border-radius: var(--radius-lg);
      border: 1px solid var(--border-light); box-shadow: var(--shadow-sm);
      margin-bottom: 20px; overflow: hidden;
    }
    .um-card-header{
      display: flex; align-items: center; justify-content: space-between;
      padding: 15px 20px; border-bottom: 1px solid var(--border-light);
    }
    .um-card-title{
      font-size: 13.5px; font-weight: 700; color: var(--text);
      display: flex; align-items: center; gap: 8px; margin: 0;
    }
    .um-card-title .fa, .um-card-title .fas{ color: var(--asyv-forest); }
    .um-card-body{ padding: 20px; }
    .um-card-body .form-group{ margin-bottom: 11px; }
    .um-card-body .form-control{ font-size: 13px !important; }
    .um-badge{
      font-size: 10.5px; font-weight: 700; padding: 3px 10px;
      border-radius: 20px; text-transform: uppercase; letter-spacing: .4px;
    }
    .um-badge-total{ background: var(--asyv-emerald-l); color: var(--asyv-forest); }
    .um-section-label{
      font-size: 11.5px; font-weight: 700; color: var(--text-secondary);
      text-transform: uppercase; letter-spacing: .45px; margin-bottom: 10px;
      display: flex; align-items: center; gap: 7px;
    }
    .um-section-label .fa, .um-section-label .fas{ color: var(--asyv-forest); font-size: 12px; }
    .um-input-row{ display: flex; gap: 8px; align-items: flex-end; }
    .um-input-row .form-group{ flex: 1; margin-bottom: 0; }
    .um-divider{ margin: 16px 0 !important; border-color: var(--border-light) !important; }
    .um-feedback{ font-size: 12.5px; margin-top: 9px; min-height: 18px; }
    .um-btn-full{ width: 100%; margin-top: 6px; }
    .um-avatar{
      width: 32px; height: 32px; border-radius: 50%;
      background: linear-gradient(135deg, var(--asyv-forest) 0%, var(--asyv-emerald) 100%);
      color: #fff; font-size: 12px; font-weight: 700;
      display: inline-flex; align-items: center; justify-content: center;
      flex-shrink: 0; text-transform: uppercase;
    }

    /* ══════════════════════════════════════════════════════════
       LOGIN PAGE
       ══════════════════════════════════════════════════════════ */
    #login-overlay{
      position: fixed; inset: 0; z-index: 9999;
      background: var(--asyv-navy);
      display: flex; align-items: center; justify-content: center;
      overflow: hidden;
    }
    /* Animated gradient orbs behind login card */
    #login-overlay::before{
      content: '';
      position: absolute;
      width: 600px; height: 600px;
      border-radius: 50%;
      background: radial-gradient(circle, rgba(11,77,59,.5) 0%, transparent 70%);
      top: -120px; right: -100px;
      animation: float1 8s ease-in-out infinite alternate;
    }
    #login-overlay::after{
      content: '';
      position: absolute;
      width: 500px; height: 500px;
      border-radius: 50%;
      background: radial-gradient(circle, rgba(212,168,67,.25) 0%, transparent 70%);
      bottom: -80px; left: -60px;
      animation: float2 10s ease-in-out infinite alternate;
    }
    @keyframes float1{
      from{ transform: translate(0, 0) scale(1); }
      to{ transform: translate(-40px, 30px) scale(1.1); }
    }
    @keyframes float2{
      from{ transform: translate(0, 0) scale(1); }
      to{ transform: translate(30px, -40px) scale(1.15); }
    }

    .login-card{
      background: rgba(255,255,255,.97);
      backdrop-filter: blur(20px);
      border-radius: var(--radius-2xl);
      padding: 48px 44px;
      width: 100%; max-width: 420px;
      box-shadow: 0 32px 64px rgba(0,0,0,.35), 0 0 0 1px rgba(255,255,255,.1);
      position: relative; z-index: 1;
      animation: cardEntry .5s cubic-bezier(.16,1,.3,1) forwards;
    }
    @keyframes cardEntry{
      from{ opacity: 0; transform: translateY(20px) scale(.97); }
      to{ opacity: 1; transform: translateY(0) scale(1); }
    }
    .login-logo{ text-align: center; margin-bottom: 36px; }
    .login-logo h2{
      color: var(--asyv-navy); font-weight: 800;
      font-size: 21px; margin: 14px 0 6px;
      letter-spacing: -.02em;
    }
    .login-logo p{
      color: var(--text-muted); font-size: 13px; margin: 0;
      font-weight: 400;
    }
    .logo-circle{
      width: 72px; height: 72px; border-radius: 20px;
      background: linear-gradient(135deg, var(--asyv-forest) 0%, var(--asyv-emerald) 100%);
      display: inline-flex; align-items: center; justify-content: center;
      color: #fff; font-size: 30px; font-weight: 800; margin-bottom: 8px;
      box-shadow: 0 8px 24px rgba(11,77,59,.35), 0 0 0 4px rgba(16,185,129,.1);
    }
    .login-card .form-group{ margin-bottom: 18px; }
    .login-card label{
      font-weight: 700; font-size: 12px; color: var(--text-secondary);
      margin-bottom: 7px; text-transform: uppercase; letter-spacing: .5px;
    }
    .login-card .form-control{
      border-radius: var(--radius) !important;
      border: 2px solid var(--border) !important;
      padding: 12px 16px !important; font-size: 14px !important;
      background: var(--bg) !important;
      transition: var(--transition) !important;
    }
    .login-card .form-control:focus{
      border-color: var(--asyv-emerald) !important;
      box-shadow: 0 0 0 4px rgba(16,185,129,.12) !important;
      background: var(--surface) !important;
    }
    #login_btn{
      width: 100%; padding: 14px; border-radius: var(--radius-lg);
      background: linear-gradient(135deg, var(--asyv-forest) 0%, var(--asyv-forest-l) 100%);
      border: none; color: #fff; font-size: 14.5px; font-weight: 700;
      margin-top: 10px; cursor: pointer; transition: var(--transition);
      box-shadow: 0 4px 14px rgba(11,77,59,.35);
      letter-spacing: .02em;
    }
    #login_btn:hover{
      transform: translateY(-1px);
      box-shadow: 0 6px 20px rgba(11,77,59,.45);
      filter: brightness(1.05);
    }
    #login_btn:active{
      transform: translateY(0);
    }
    #login_error{
      color: #DC2626; font-size: 13px; text-align: center;
      margin-top: 14px; display: none;
      background: #FEF2F2; padding: 10px; border-radius: var(--radius);
      border: 1px solid #FECACA;
    }
    .pw-wrapper{ position: relative; }
    .pw-toggle{
      position: absolute; right: 14px; top: 50%; transform: translateY(-50%);
      cursor: pointer; color: var(--text-muted); font-size: 14px; z-index: 10;
      transition: var(--transition);
    }
    .pw-toggle:hover{ color: var(--text-secondary); }

    /* ══════ SECTION HEADERS ═════════════════════════════════ */
    h5{ color: var(--text); font-weight: 700 !important; font-size: 15px !important; }
    h6{ color: var(--text-secondary); font-weight: 600 !important; font-size: 13px !important; }

    /* ══════ RESPONSIVE ══════════════════════════════════════ */
    @media (max-width: 768px) {
      .content{ padding: 14px 12px !important; }
      .login-card{ padding: 32px 24px; margin: 0 16px; }
    }

    /* ══════ USER MANAGEMENT — ACTION BUTTONS ════════════════ */
    .um-action-btn{
      background: none; border: 1px solid var(--border);
      border-radius: 6px; padding: 3px 10px;
      font-size: 11.5px; font-weight: 600;
      cursor: pointer; transition: var(--transition);
      font-family: inherit;
    }
    .um-action-btn:hover{ box-shadow: var(--shadow-sm); }
    .um-edit-btn{ color: var(--asyv-forest); }
    .um-edit-btn:hover{ background: var(--asyv-emerald-l); border-color: var(--asyv-forest); }
    .um-toggle-deactivate{ color: #DC2626; margin-left: 5px; }
    .um-toggle-deactivate:hover{ background: #FEE2E2; border-color: #DC2626; }
    .um-toggle-activate{ color: #059669; margin-left: 5px; }
    .um-toggle-activate:hover{ background: #D1FAE5; border-color: #059669; }

    /* ══════ STICKY FILTER SIDEBAR ═══════════════════════════ */
    .tab-pane .row > .col-sm-3 {
      position: sticky;
      top: 20px;
      align-self: flex-start;
    }
  "))),

  tags$head(tags$script(HTML("
    $(document).on('click', '.um-edit-btn', function(){
      Shiny.setInputValue(
        'edit_user_click',
        { username: $(this).data('username'), nonce: Math.random() },
        { priority: 'event' }
      );
    });
    $(document).on('click', '.um-toggle-btn', function(){
      Shiny.setInputValue(
        'toggle_user_click',
        { username: $(this).data('username'), nonce: Math.random() },
        { priority: 'event' }
      );
    });

    /* ── Sidebar treeview — delegated so it survives renderUI redraws ── */
    $(document).on('click', '.nav-sidebar .nav-item.has-treeview > a.nav-link', function(e) {
      e.preventDefault();
      e.stopPropagation();
      var $item = $(this).closest('.nav-item.has-treeview');
      var $sub  = $item.find('> ul.nav-treeview');
      if ($item.hasClass('menu-open')) {
        $item.removeClass('menu-open');
        $sub.slideUp(200);
      } else {
        /* collapse any open sibling first (accordion) */
        $item.siblings('.has-treeview.menu-open').each(function(){
          $(this).removeClass('menu-open').find('> ul.nav-treeview').slideUp(200);
        });
        $item.addClass('menu-open');
        $sub.slideDown(200);
      }
    });
  "))),

  # ── LOGIN OVERLAY ──────────────────────────────────────────
  div(id="login-overlay",
      div(class="login-card",
          div(class="login-logo",
              div(class="logo-circle","A"),
              h2("ASYV Analytics"),
              p("Agahozo-Shalom Youth Village \u2014 Departmental Analytics")),
          div(class="form-group",
              tags$label("Username"),
              textInput("login_user",label=NULL,placeholder="Enter your username")),
          div(class="form-group",
              tags$label("Password"),
              div(class="pw-wrapper",
                  passwordInput("login_pass",label=NULL,placeholder="Enter your password"),
                  span(class="pw-toggle",id="pw_eye",icon("eye")))),
          actionButton("login_btn","Sign In",icon=icon("right-to-bracket")),
          div(id="login_error",icon("circle-exclamation")," Invalid username or password.")
      )
  ),

  # ── MAIN APP ───────────────────────────────────────────────
  div(id="main-app",style="display:none;",
      bs4DashPage(title="ASYV Analytics Dashboard",dark=FALSE,
                  header=bs4DashNavbar(
                    title=bs4DashBrand(title="ASYV Analytics",color="primary"),
                    skin="dark",status="navy",border=FALSE,
                    rightUi=tagList(
                      tags$li(class="nav-item dropdown", uiOutput("navbar_user")),
                      tags$li(class="nav-item dropdown",
                              actionButton("logout_btn","",icon=icon("right-from-bracket"),
                                           title="Sign out")))
                  ),
                  sidebar=bs4DashSidebar(skin="light",status="primary",elevation=2,
                                         uiOutput("sidebar_menu")
                  ),
                  body=bs4DashBody(
                    do.call(bs4TabItems, c(
                      dept_tab_items,
                      list(

                    # USER MANAGEMENT (admin only) ──────────────────────
                    bs4TabItem(tabName="admin_users",

                      # ── Stat cards ──────────────────────────────────
                      fluidRow(
                        column(4, div(class="um-stat-card",
                          div(class="um-stat-icon um-icon-forest", icon("users")),
                          div(div(class="um-stat-label","Total Users"),
                              div(class="um-stat-value", uiOutput("um_total",inline=TRUE))))),
                        column(4, div(class="um-stat-card",
                          div(class="um-stat-icon um-icon-green", icon("circle-check")),
                          div(div(class="um-stat-label","Active"),
                              div(class="um-stat-value", uiOutput("um_active",inline=TRUE))))),
                        column(4, div(class="um-stat-card",
                          div(class="um-stat-icon um-icon-gold", icon("shield-halved")),
                          div(div(class="um-stat-label","Administrators"),
                              div(class="um-stat-value", uiOutput("um_admins",inline=TRUE)))))
                      ),

                      # ── Full-width table ─────────────────────────────
                      fluidRow(
                        column(12,
                          div(class="um-card",
                            div(class="um-card-header",
                              p(class="um-card-title", icon("users"), "Team Members"),
                              actionButton("btn_open_add", "New Member",
                                icon  = icon("user-plus"),
                                class = "btn btn-sm",
                                style = paste0(
                                  "background:var(--asyv-forest);color:#fff;border:none;",
                                  "font-size:12.5px;font-weight:600;",
                                  "padding:6px 14px;border-radius:8px;"
                                )
                              )
                            ),
                            div(class="um-card-body", style="padding:0;",
                              DTOutput("users_table")
                            )
                          )
                        )
                      )
                    ),

                    # MY ACCOUNT ────────────────────────────────────────
                    bs4TabItem(tabName="my_account",
                               fluidRow(bs4Card(title=tagList(icon("user-circle")," My Account"),width=6,
                                                status="info",solidHeader=TRUE,
                                                uiOutput("account_info"),hr(),
                                                h6(style="font-weight:600;","Change password"),
                                                passwordInput("cur_password","Current password"),
                                                passwordInput("new_pw1","New password"),
                                                passwordInput("new_pw2","Confirm new password"),
                                                actionButton("btn_change_pw","Update Password",icon=icon("lock"),class="btn-primary"),
                                                div(id="change_pw_msg",style="margin-top:8px;font-size:13px;")))
                    )

                    )
                  ))
                  ),
                  footer=bs4DashFooter(left="ASYV Analytics Dashboard \u00b7 Agahozo-Shalom Youth Village",right="")
      )
  )
)
