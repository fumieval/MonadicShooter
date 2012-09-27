type Enemy = BulletT (Yield (Vec2, Double)) (ReaderT Vec2 (WriterT Picture IO)) ()

